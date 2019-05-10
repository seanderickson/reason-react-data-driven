open Belt;
open Common;
open Store;

type state = webLoadingData(option(array(Js.Json.t)));
let initialState = NotAsked;

[@react.component]
let make = (~resource: resource )=> {

  let (state, setState) = React.useState(() => initialState);

  let fetchEntities = () => {
    setState(_=>Loading);
    ApiClient.getEntityListing(resource.name)
    |> Js.Promise.then_(result => {
      switch(result) {
        | Result.Ok(entities) => setState(_ => LoadSuccess(Some(entities)))
        | Result.Error(message) => setState(_=>LoadFailure(message))
        };
        Js.Promise.resolve();
      })
    |> ignore;
  };

  React.useEffect1(() => {
    fetchEntities();
    Some(() => {
      Js.log("cleanup Effect");
    });
  }, [|resource.name|]);

  
  let handleRoute = (endpoint, id, evt) => {
    ReactEvent.Synthetic.preventDefault(evt);
    ReasonReactRouter.push(
      "/" ++ Js.Array.joinWith("/", [|endpoint, id|]));
  };

  let printEntities = entities => {
    <div>
      <h3>(str("Entity Listing: " ++ resource.title))</h3>
      <form className="grid_table" id="entity_table" >
        <div key="header" className="grid_table_row grid_table_header" >
          (resource.fields
          |> Array.map(_, field => {
            <label key=field.name className="grid_table_row_cell" >(str(field.title))</label>
          })
          |> ReasonReact.array)
        </div>
      (entities
      |> Array.mapWithIndex(_, (i,entity) => {
        let fieldReader = Store.Decode.fieldDecoder(entity, resource);
        <div key=(string_of_int(i)) className="grid_table_row" >
          (resource.fields
          |> Array.map(_, field => {
            let fvalue = fieldReader(field.name);
            <div key=field.name className="grid_table_row_cell">
              (switch(field.ref_endpoint) {
                | Some(endpoint) => 
                      <a onClick=handleRoute(endpoint, fvalue) href=("/" ++ endpoint ++ "/" ++ fvalue) >(str(fvalue))</a>
                | None => (str(fvalue))
              })
            </div>
          })
          |> ReasonReact.array)
        </div>
      })
      |> ReasonReact.array)
      </form>
    </div>
  };

  <div id="entities">
    <button onClick=(_=>fetchEntities()) >(str("Fetch: " ++ resource.title))</button>
    <br/>
    (switch(state) {
      | NotAsked => (str("Not asked..."))
      | LoadFailure(msg) => (str("Load failure: " ++ msg))
      | Loading => (str("Loading..."))
      | LoadSuccess(entities) => switch(entities) {
          | Some(entities) => entities -> printEntities
          | None => str("No entities found")
      }
    })
  </div>
};
