
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

type field = {
  // id: int,
  // Resource containing this field
  resource_name: string,
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
      // id: json |> field("id", int),
      resource_name: json |> field("resource_name", string),
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
        ApiClient.buildResources()
        |> Js.Promise.then_(result => {
            switch (result) {
            | Result.Ok(resources) => setResourceState(_=>{...initialResourceState, resources: Some(resources)})
            | Result.Error(message) => setResourceState(_=>{...initialResourceState, error: Some(message)})
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

