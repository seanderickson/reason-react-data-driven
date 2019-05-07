
[%%debugger.chrome];

open Belt;


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

type field = {
  id: int,
  // Resource containing this field
  resource_id: int,
  name: string,
  title: string,
  description: string,
  // JSON data type: string, number, boolean, array, (object - not used?)
  data_type: string,
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

type resourceState = {
  // user: option(user),
  // users: option(listResponse(user)),
  // listing: option(array(string)),
  resources: option(resources),
  error: option(string),
  isLoading: bool
};

module Decode = {

  let readField = json => 
    Json.Decode.{
      id: json |> field("id", int),
      resource_id: json |> field("resource_id", int),
      name: json |> field("name", string),
      title: json |> field("title", string),
      description: json |> field("description", string),
      data_type: json |> field("data_type", string),
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
};
type apiResult('a) = Js.Promise.t(Result.t('a, string));

exception BadStatus(string);

module ApiClient = {
  // let checkStatus = response => {

  // };
  let getFields = () : apiResult(array(field)) => {
    Js.Promise.(
      Fetch.fetch(apiUrl ++ "/field")
      |> then_(Fetch.Response.json)
      |> then_(json => {
          resolve(Result.Ok(Decode.readFields(json)));
         })
      |> catch(err => resolve(Result.Error({j|API error (error=$err)|j})))
    );
  };
  let getResources = () : apiResult(array(resource)) => {
    Js.Promise.(
      Fetch.fetch(apiUrl ++ "/resource")
      |> then_(response=>{
        let status = response->Fetch.Response.status;
        let statusText = response->Fetch.Response.statusText;
        if ( ! response -> Fetch.Response.ok){
          // NOTE: must raise an exception, resolve, or return another promise:
          // - is there another way to exit?
          // reject(BadStatus("Some failure, status: " ++ string_of_int(status) ++ ": " ++ statusText));
          // raise(BadStatus("Some failure, status: " ++ string_of_int(status) ++ ": " ++ statusText));
          resolve(Result.Error({j|Connection failed: status=$status, $statusText |j}));
        } else {
          response 
          |> Fetch.Response.json 
          |> then_(json => resolve(Result.Ok(Decode.readResources(json))));
        }
      })
      // |> then_(json => {
      //     resolve(Result.Ok(Decode.readResources(json)));
      //    })
      |> catch(err => resolve(Result.Error({j|API error (error=$err)|j})))
    );
  };

  let getResources = () : apiResult(array(resource)) => {
    Js.Promise.(
      Fetch.fetch(apiUrl ++ "/resource")
      |> then_(Fetch.Response.json)
      |> then_(json => {
          resolve(Result.Ok(Decode.readResources(json)));
         })
      |> catch(err => resolve(Result.Error({j|API error (error=$err)|j})))
    );
  };

};

let initialResourceState:resourceState = {
  resources: None,
  error: None,
  isLoading: true
  };


module ResourceContext = {
  type t = {
    resourceState: resourceState,
    fetchResources: unit => unit,
    setResources: unit => unit,
  };

  let reactContext = React.createContext(None);


  module Provider = {
    [@react.component]
    let make = (~children) => {
      let (resourceState, setResourceState) = React.useState(() => initialResourceState);

      let fetchResources = () => {
        Js.log("handle click...");
        ApiClient.getResources()
        |> Js.Promise.then_(result => {
            switch (result) {
            | Result.Ok(resources) => setResourceState(_=>{...initialResourceState, resources: Some(resources)})
            | Result.Error(message) => setResourceState(_=>{...initialResourceState, error: Some(message)})
            };
            Js.Promise.resolve();
          })
        |> ignore;
      };

      let ctx: option(t) =
        Some({
          resourceState,
          fetchResources: fetchResources,
          setResources: () => (),
        })
        |> (it => React.useMemo1(() => it, [| resourceState |]));

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


// let context = React.createContext(initialResourceState);

// module ContextProvider = {
//   let make = context->React.Context.provider;

//   [@bs.obj]
//   external makeProps:
//     (~value: resourceState, ~children: React.element, ~key: string=?, unit) =>
//     {
//       .
//       "value": resourceState,
//       "children": React.element,
//     } =
//     "";
// };