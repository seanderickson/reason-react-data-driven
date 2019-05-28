[%%debugger.chrome];

open Belt;

// Example of exhaustive typing...
// type irb = {
//   id: int,
//   funding: string,
//   type_: string,
//   lab: string,
//   secondary_irb: string
// }
// and sample = {
//   id: int,
//   project_no: int,
//   primary_irb_id:  int,
//   secondary_irb_id: int,
//   custodian_user_id: int,
//   organism: string,
//   tissue: string,
//   pathology: string,
//   case_no: int,
//   slide_no: int,
//   stain_type: string,
//   image_url: string,
//   slide_id: string,
//   inventory_date: string,
//   slide_location: string,
//   assigned_user_id: int,
//   checkout_date: string,
//   initial_barcode: string,
//   leica_barcode: string,
//   leica_name: string,
//   experiment_id: string,
//   comments: string
// }
// and project = {
//   id: int,
//   name: string,
//   principal_investigator_id: int,
//   protocol: string,
//   scientist_id: int,
//   scientist_conducting: int,
//   grant: string,
//   purpose: string,
//   organism: string
// };

let apiUrl = "http://localhost:3000";

type dataType =
  | String
  | Integer
  | Boolean
  | ArrayString;

// Try out poly variant: the jsConverter generates the "validatorFromJs" function
[@bs.deriving jsConverter]
type validator = [ | `required | `name | `email];

type vocabulary = {
  // id: int,
  scope: string,
  key: string,
  title: string,
  description: option(string),
}
and vocabularies = array(vocabulary)
and field = {
  // id: int,
  // Resource containing this field
  resource_name: string,
  name: string,
  title: string,
  description: string,
  data_type: dataType,
  // Display type; may imply conversion, e.g. string => date
  display_type: string,
  // If field refers to another entity; endpoint for that entity
  ref_endpoint: option(string),
  validators: option(array(validator)),
  editable: bool,
  vocab_scope: option(string),
  vocabularies: option(vocabularies),
}
and resource = {
  id: int,
  name: string,
  title: string,
  description: string,
  fields: array(field),
}
and fields = array(field)
and resources = array(resource);

exception DecodeTypeException(string);

module Decode = {
  let readVocab = (json): vocabulary => {
    Js.log2("decoding field: ", json);
    Json.Decode.{
      key: json |> field("key", string),
      scope: json |> field("scope", string),
      title: json |> field("title", string),
      description: json |> optional(field("description", string)),
    };
  };
  let readVocabularies = json => Json.Decode.(json |> array(readVocab));
  let readField = json => {
    Js.log2("decoding field: ", json);
    Json.Decode.{
      // id: json |> field("id", int),
      resource_name: json |> field("resource_name", string),
      name: json |> field("name", string),
      title: json |> field("title", string),
      description: json |> field("description", string),
      data_type:
        json
        |> field("data_type", string)
        |> (
          v =>
            switch (v) {
            | "string" => String
            | "integer" => Integer
            | "arraystring" => ArrayString
            | "boolean" => Boolean
            | unknownType =>
              raise(
                DecodeTypeException(
                  "Missing type conversion case for field data_type: "
                  ++ unknownType
                  ++ ", JSON: "
                  ++ Js.Json.stringify(json),
                ),
              )
            }
        ),
      display_type: json |> field("display_type", string),
      ref_endpoint: json |> optional(field("ref_endpoint", string)),
      validators:
        json
        |> optional(field("validators", array(string)))
        |> Belt.Option.map(_, arrayString =>
             Belt.Array.map(arrayString, v =>
               validatorFromJs(v)->Belt.Option.getExn
             )
           ),
      editable:
        json
        |> optional(field("editable", bool))
        |> Belt.Option.getWithDefault(_, true),
      vocab_scope: json |> optional(field("vocab_scope", string)),
      vocabularies: None,
    };
  };

  let readFields = json => Json.Decode.(json |> array(readField));
  let readResource = json => {
    Js.log2("decoding resource: ", json);
    Json.Decode.{
      id: json |> field("id", int),
      name: json |> field("name", string),
      title: json |> field("title", string),
      description: json |> field("description", string),
      fields: [||],
    };
  };
  let readResources = json => Json.Decode.(json |> array(readResource));

  let getField = (resource, fieldName) => {
    resource.fields |> Array.getBy(_, field => field.name == fieldName);
  };

  let fieldDecoderExn = (json: Js.Json.t, schemaField: field): string => {
    // let f = getField(resource, fieldName);
    Json.Decode.(
      switch (schemaField.data_type) {
      | String =>
        json
        |> optional(field(schemaField.name, string))
        |> Belt.Option.getWithDefault(_, "-")
      | Integer =>
        json
        |> optional(field(schemaField.name, int))
        |> (
          opt =>
            switch (opt) {
            | Some(x) => string_of_int(x)
            | None => "-"
            }
        )
      | ArrayString =>
        json
        |> optional(field(schemaField.name, array(string)))
        |> Belt.Option.mapWithDefault(_, "-", v =>
             v |> Js.Array.joinWith(", ")
           )
      | Boolean =>
        json
        |> optional(field(schemaField.name, bool))
        |> Belt.Option.getWithDefault(_, true)
        |> string_of_bool
      | _ => {j|unknown type for field: "$schemaField.name" - "$schemaField.data_type" |j}
      }
    );
  };

  let fieldDecoder = (json, field: field): string =>
    try (
      {
        fieldDecoderExn(json, field);
      }
    ) {
    | Js.Exn.Error(e) =>
      switch (Js.Exn.message(e)) {
      | Some(message) => {j|Error: $message|j}
      | None => "An unknown error occurred"
      }
    | Json.Decode.DecodeError(msg) => msg
    };

  let singleFieldDecode = (json: Js.Json.t, schemaField: field): string => {
    switch (schemaField.data_type) {
    | String => json |> Json.Decode.string
    | Integer => json |> Json.Decode.int |> string_of_int
    | ArrayString =>
      json |> Json.Decode.(array(string)) |> Js.Array.joinWith(", ")
    | Boolean => json |> Json.Decode.(bool) |> string_of_bool
    };
  };

  let nullDecoder = json => json;
  let jsonArrayDecoder = Json.Decode.array(nullDecoder);
};

module Encode = {
  exception UndefinedEncoder(string);

  let encodeField = (field: field, formValue: string): Js.Json.t =>
    switch (field.data_type) {
    | String => formValue |> Js.Json.string
    | Integer => formValue |> float_of_string |> Js.Json.number
    | Boolean => formValue |> bool_of_string |> Js.Json.boolean
    | _ =>
      raise(UndefinedEncoder({j|Encoder for "$field.name" is not defined|j}))
    };
};

module ApiClient = {
  type apiResult('a) = Js.Promise.t(Result.t('a, string));

  let fetch = (url, decoder): apiResult('a) => {
    Js.log2("fetching: ", url);
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
             Js.log("Status ok, decoding json...");
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

  let test_mock_error_mode = false;

  let postPatch = (url, method, decoder, payload: Js.Json.t): apiResult('a) => {
    Js.log2("posting: ", url);

    Js.Promise.(
      Fetch.fetchWithInit(
        url,
        Fetch.RequestInit.make(
          ~method_=method,
          ~body=Fetch.BodyInit.make(Js.Json.stringify(payload)),
          ~headers=
            Fetch.HeadersInit.make({"Content-Type": "application/json"}),
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
           resolve(
             Result.Error({j|API POST error(URL: $url, error=$err)|j}),
           );
         })
    );
  };

  exception BadStatus(string);

  let getVocabularies = () =>
    fetch(apiUrl ++ "/vocabulary", Decode.readVocabularies);

  let getFields = () => fetch(apiUrl ++ "/field", Decode.readFields);

  let getResources = () => fetch(apiUrl ++ "/resource", Decode.readResources);

  let assembleField = (field, vocabs): field => {
    switch (field.vocab_scope) {
    | Some(vocab_scope) => {
        ...field,
        vocabularies:
          Some(vocabs |> Js.Array.filter(v => v.scope == vocab_scope)),
      }
    | None => field
    };
  };
  let assembleResources = (resources: resources, fields, vocabs): resources => {
    let assembledFields: fields =
      Array.map(fields, field => assembleField(field, vocabs));

    resources
    |> Array.map(_, resource =>
         {
           ...resource,
           fields:
             assembledFields
             |> Js.Array.filter(f => f.resource_name == resource.name),
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
                         Js.Promise.resolve(Result.Error(message))
                       }
                     )
                | Result.Error(message) =>
                  Js.Promise.resolve(Result.Error(message))
                }
              )
         | Result.Error(message) =>
           Js.Promise.resolve(Result.Error(message))
         }
       );

  let getEntityListing = resourceName =>
    fetch(apiUrl ++ "/" ++ resourceName, Decode.jsonArrayDecoder);

  let getEntity = (resourceName, id) =>
    fetch(apiUrl ++ "/" ++ resourceName ++ "/" ++ id, Decode.nullDecoder);

  let postEntity = (resourceName, payload) =>
    postPatch(
      apiUrl ++ "/" ++ resourceName,
      Fetch.Post,
      Decode.nullDecoder,
      payload,
    );

  let patchEntity = (resourceName, id, payload) =>
    postPatch(
      apiUrl ++ "/" ++ resourceName ++ "/" ++ id,
      Fetch.Patch,
      Decode.nullDecoder,
      payload,
    );
};

type webLoadingData('a) =
  | NotAsked
  | Loading
  | LoadFailure(string)
  | LoadSuccess('a);

type resourceState = webLoadingData(option(resources));
let initialResourceState = NotAsked;

module ResourceContext = {
  type t = {
    resourceState,
    fetchResources: unit => unit,
    setResources: unit => unit,
  };

  let reactContext = React.createContext(None);

  module Provider = {
    [@react.component]
    let make = (~children) => {
      let (resourceState, setResourceState) =
        React.useState(() => initialResourceState);

      let fetchResources = () => {
        setResourceState(_ => Loading);
        ApiClient.buildResources()
        |> Js.Promise.then_(result => {
             switch (result) {
             | Result.Ok(resources) =>
               Js.log2("Built resources:", resources);
               setResourceState(_ => LoadSuccess(Some(resources)));
             | Result.Error(message) =>
               setResourceState(_ => LoadFailure(message))
             };
             Js.Promise.resolve();
           })
        |> ignore;
      };

      React.useEffect1(
        () => {
          Js.log("Initial fetch...");
          fetchResources();
          Some(() => Js.log("cleanup Effect"));
        },
        [||],
      );

      let ctx: option(t) =
        Some({resourceState, fetchResources, setResources: () => ()})
        |> (it => React.useMemo1(() => it, [|resourceState|]));
      // TODO: useMemo1 may require a shallow compare
      Common.reactContextProvider(
        ~children,
        ~context=reactContext,
        ~value=ctx,
      );
    };
  };

  exception ResourceContextNotFound;

  let useResources = () => {
    switch (React.useContext(reactContext)) {
    | Some(ctx) => ctx
    | None => raise(ResourceContextNotFound)
    };
  };
};