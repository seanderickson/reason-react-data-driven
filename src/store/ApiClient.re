open Belt;
open Metadata;

let debug_mode = false;
let test_mock_error_mode = false;

let apiUrl = "http://localhost:3000";

type apiResult('a) = Js.Promise.t(Result.t('a, string));

let fetch = (url, decoder): apiResult('a) => {
  if (debug_mode) {
    Js.log2("fetching: ", url);
  };
  Js.Promise.(
    Fetch.fetch(url)
    |> then_(response => {
         let status = response->Fetch.Response.status;
         let statusText = response->Fetch.Response.statusText;
         if (!response->Fetch.Response.ok) {
           resolve(
             Result.Error(
               {j|Response Error: status=$status, "$statusText" |j},
             ),
           );
         } else {
           response
           |> Fetch.Response.json
           |> then_(json => resolve(Result.Ok(decoder(json))));
         };
       })
    |> catch(err => {
         Js.log2("error", err);
         resolve(Result.Error({j|API error (URL: $url, error=$err)|j}));
       })
  );
};

let postPatch = (url, method, decoder, payload: Js.Json.t): apiResult('a) => {
  Js.log2("posting: ", url);

  Js.Promise.(
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=method,
        ~body=Fetch.BodyInit.make(Js.Json.stringify(payload)),
        ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
        (),
      ),
    )
    |> then_(response => {
         let status = response->Fetch.Response.status;
         let statusText = response->Fetch.Response.statusText;
         if (!response->Fetch.Response.ok) {
           resolve(
             Result.Error(
               {j|Response Error: status=$status, "$statusText" |j},
             ),
           );
         } else if (test_mock_error_mode) {
           let mockFieldError = {j|{
            "protocol_io": { "Unknown protocol": "This protocol is not registered", "Message2": "second message" },
            "primary_contact": { "Duplicate": "Name is already used" } }
            |j};
           resolve(Result.Error(mockFieldError));
         } else {
           Js.log("Status ok, decoding json...");
           response
           |> Fetch.Response.json
           |> then_(json => resolve(Result.Ok(decoder(json))));
         };
       })
    |> catch(err => {
         Js.log2("post error", err);
         resolve(Result.Error({j|API POST error(URL: $url, error=$err)|j}));
       })
  );
};

exception BadStatus(string);

let getVocabularies = () =>
  fetch(apiUrl ++ "/vocabulary", Vocabulary.decodeMany);

let getFields = () => fetch(apiUrl ++ "/field", Field.decodeMany);

let getResources = () => fetch(apiUrl ++ "/resource", Resource.decodeMany);

let assembleField = (field: Field.t, vocabs): Field.t => {
  switch (field.vocab_scope) {
  | Some(vocab_scope) => {
      ...field,
      vocabularies:
        Some(
          vocabs |> Js.Array.filter(v => v.Vocabulary.scope == vocab_scope),
        ),
    }
  | None => field
  };
};
let assembleResources =
    (resources: Resource.resources, fields, vocabs): Resource.resources => {
  let assembledFields: Field.fields =
    Array.map(fields, field => assembleField(field, vocabs));

  resources
  |> Array.map(_, resource =>
       {
         ...resource,
         fields:
           assembledFields
           |> Js.Array.filter(f => f.Field.resource_name == resource.name),
       }
     );
};

let buildResources = () =>
  getVocabularies()
  |> Js.Promise.then_(resultv =>
       switch (resultv) {
       | Result.Ok(vocabs) =>
         getFields()
         |> Js.Promise.then_(result1 =>
              switch (result1) {
              | Result.Ok(fields) =>
                getResources()
                |> Js.Promise.then_(result =>
                     switch (result) {
                     | Result.Ok(resources) =>
                       Js.Promise.resolve(
                         Result.Ok(
                           assembleResources(resources, fields, vocabs),
                         ),
                       )
                     | Result.Error(message) =>
                       Js.Promise.resolve(
                         Result.Error("Resource fetch: " ++ message),
                       )
                     }
                   )
              | Result.Error(message) =>
                Js.Promise.resolve(Result.Error("Field fetch: " ++ message))
              }
            )
       | Result.Error(message) =>
         Js.Promise.resolve(Result.Error("Vocabulary fetch: " ++ message))
       }
     );

let getEntityListing = resourceName =>
  fetch(apiUrl ++ "/" ++ resourceName, jsonArrayDecoder);

let getEntity = (resourceName, id) =>
  fetch(apiUrl ++ "/" ++ resourceName ++ "/" ++ id, nullDecoder);

let postEntity = (resourceName, payload) =>
  postPatch(apiUrl ++ "/" ++ resourceName, Fetch.Post, nullDecoder, payload);

let patchEntity = (resourceName, id, payload) =>
  postPatch(
    apiUrl ++ "/" ++ resourceName ++ "/" ++ id,
    Fetch.Patch,
    nullDecoder,
    payload,
  );