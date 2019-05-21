
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
  | Integer;

type field = {
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
  ref_endpoint: option(string)
} 
and 
resource = {
  id: int,
  name: string,
  title: string,
  description: string,
  fields: array(field)
} 
and fields = array(field)
and resources = array(resource);

exception DecodeTypeException(string);

module Decode = {

  let readField = json => 
    Json.Decode.{
      // id: json |> field("id", int),
      resource_name: json |> field("resource_name", string),
      name: json |> field("name", string),
      title: json |> field("title", string),
      description: json |> field("description", string),
      data_type: json |> field("data_type", string) |> (v) => 
       switch(v){
        | "string" => String
        | "integer" => Integer
        | unknownType => raise(DecodeTypeException(unknownType))
      },
      display_type: json |> field("display_type", string),
      ref_endpoint: json |> optional(field("ref_endpoint", string))
    };
  let readFields = json => Json.Decode.(json |> array(readField));
  let readResource = json => 
    Json.Decode.{
      id: json |> field("id", int),
      name: json |> field("name", string),
      title: json |> field("title", string),
      description: json |> field("description", string),
      fields: [||]
    };
  let readResources = json => Json.Decode.(json |> array(readResource));

  let getField = (resource, fieldName) => {
    resource.fields
    |> Array.getBy(_, field => field.name==fieldName);
  };

  let fieldDecoderExn = (json, schemaField:field):string => {
    // let f = getField(resource, fieldName);
    open Json.Decode;
    switch(schemaField.data_type) {
      | String => json 
        |> optional(field(schemaField.name, string))
        |> Belt.Option.getWithDefault(_,"-")
      | Integer => json 
        |> optional(field(schemaField.name, int))
        |> opt => switch(opt){
          | Some(x) => string_of_int(x)
          | None => "-"
        }
      | _ => {j|unknown type for field: "$schemaField.name" - "$schemaField.data_type" |j}
    }
  };

  let fieldDecoder = (json, field:field):string => {
    try {
      fieldDecoderExn(json, field);
    } {
    | Js.Exn.Error(e) =>
      switch (Js.Exn.message(e)) {
      | Some(message) => {j|Error: $message|j}
      | None => "An unknown error occurred"
      }
    | Json.Decode.DecodeError(msg) => msg
    };
  };

  let singleFieldDecode = (json:Js.Json.t, schemaField:field):string => {
    switch(schemaField.data_type) {
      | String => json |> Json.Decode.string
      | Integer => json |> Json.Decode.int |> string_of_int
    };
  };

  let nullDecoder = (json) => json;
  let jsonArrayDecoder = Json.Decode.array(nullDecoder);
};

module Encode = {

  let fieldEncoder = (dict:Js.Dict.t(Js.Json.t), field:field, value:string) => {
      switch(field.data_type) {
        | String => {
          Js.Dict.set(dict, field.name, value |> Js.Json.string);
        }
        | Integer => {
          Js.Dict.set(dict, field.name, float_of_string(value) |> Js.Json.number);
        }
      }
      dict;
  };

};

type apiResult('a) = Js.Promise.t(Result.t('a, string));

let fetch = (url, decoder): apiResult('a) => {
  Js.log2("fetching: ", url);
  Js.Promise.(
    Fetch.fetch(url)
    |> then_(response=>{
      let status = response->Fetch.Response.status;
      let statusText = response->Fetch.Response.statusText;
      if ( ! response -> Fetch.Response.ok){
        resolve(Result.Error({j|Response Error: status=$status, "$statusText" |j}));
      } else {
        response 
        |> Fetch.Response.json 
        |> then_(json => resolve(Result.Ok(decoder(json))));
      }
    })
    |> catch(err =>{
      resolve(Result.Error({j|API error (error=$err)|j}));
    })
  );
};

exception BadStatus(string);

module ApiClient = {
  
  let getFields = () => fetch(apiUrl ++ "/field", Decode.readFields);

  let getResources = () => fetch(apiUrl ++ "/resource", Decode.readResources);

  let buildResources = () => {
    getFields()
    |> Js.Promise.then_(result => {
        switch (result) {
          | Result.Ok(fields) =>{
            getResources()
            |> Js.Promise.then_(result1 =>{
              switch(result1) {
                | Result.Ok(resources:resources) =>
                    Js.Promise.resolve(Result.Ok(
                      resources 
                      -> Array.map(resource => {
                          {...resource, 
                            fields: fields 
                              |> Js.Array.filter(f => f.resource_name==resource.name)}
                        })
                    ))
                | Result.Error(message) => Js.Promise.resolve(Result.Error(message))
              }
            })
          }
          | Result.Error(message) => Js.Promise.resolve(Result.Error(message))
        };
      })
  };
  let getEntityListing = (resourceName) =>  fetch(apiUrl ++ "/" ++ resourceName, Decode.jsonArrayDecoder);

  let getEntity = (resourceName, id) => fetch(apiUrl ++ "/" ++ resourceName ++ "/" ++ id, Decode.nullDecoder)

  // let getResources = () : apiResult(array(resource)) => {
  //   Js.Promise.(
  //     Fetch.fetch(apiUrl ++ "/resource")
  //     |> then_(Fetch.Response.json)
  //     |> then_(json => {
  //         resolve(Result.Ok(Decode.readResources(json)));
  //        })
  //     |> catch(err => resolve(Result.Error({j|API error (error=$err)|j})))
  //   );
  // };

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

      let (resourceState, setResourceState) = React.useState(() => initialResourceState);
  
      let fetchResources = () => {
        setResourceState(_=>Loading);
        ApiClient.buildResources()
        |> Js.Promise.then_(result => {
            switch (result) {
            | Result.Ok(resources) => setResourceState(_=>LoadSuccess(Some(resources)))
            | Result.Error(message) => setResourceState(_=>LoadFailure(message))
            };
            Js.Promise.resolve();
          })
        |> ignore;
      };

      React.useEffect1(() => {
        Js.log("Initial fetch...");
        fetchResources();
        Some(() => {
          Js.log("cleanup Effect");
        });
      }, [||]);

      let ctx: option(t) =
        Some({
          resourceState,
          fetchResources: fetchResources,
          setResources: () => (),
        })
        |> (it => React.useMemo1(() => it, [| resourceState |]));
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

