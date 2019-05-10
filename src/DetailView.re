open Belt;
open Common;
open Store;

type state = webLoadingData(option(Js.Json.t));
let initialState = NotAsked;

[@react.component]
let make = (~resource: resource, ~id: string, ~initialEntity=?, () )=> {

  let (state, setState) = React.useState(() => Belt.Option.getWithDefault(initialEntity, initialState));

  let fetchEntity = () => {
    setState(_=>Loading);
    ApiClient.getEntity(resource.name, id)
    |> Js.Promise.then_(result => {
      switch(result) {
        | Result.Ok(entity) => setState(_ => LoadSuccess(Some(entity)))
        | Result.Error(message) => setState(_=>LoadFailure(message))
        };
        Js.Promise.resolve();
      })
    |> ignore;
  };

  React.useEffect1(() => {
    if (state == NotAsked) fetchEntity();
    Some(() => {
      Js.log("cleanup Effect");
    });
  }, [|resource.name|]);

  
  let handleRoute = (endpoint, id, evt) => {
    ReactEvent.Synthetic.preventDefault(evt);
    ReasonReactRouter.push(
      "/" ++ Js.Array.joinWith("/", [|endpoint, id|]));
  };

  let printEntity = entity => {
    let fieldReader = Store.Decode.fieldDecoder(entity, resource);
    
    <div>
    <h3>(str("Entity detail: " ++ resource.title ++ ": " ++ id))</h3>
    <form className="grid_table" id="entity_table" >
      <div key="header" className="grid_table_row grid_table_header" >
        <div key="fieldTitle" className="grid_table_row_cell" >(str("Field"))</div>    
        <div key="fieldValue" className="grid_table_row_cell" >(str("Value"))</div>    
      </div>

      (resource.fields
      |> Array.map(_, field => {
        let fvalue = fieldReader(field.name);

        <div key=("row"++field.name) className="grid_table_row " >
          <label key="title" className="grid_table_row_cell" >(str(field.title))</label>
          <div key="value" className="grid_table_row_cell">
          (switch(field.ref_endpoint) {
            | Some(endpoint) => 
                <a onClick=handleRoute(endpoint, fvalue) href=("/" ++ endpoint ++ "/" ++ fvalue) >(str(fvalue))</a>
            | None => (str(fvalue))
          })
          </div>
        </div>
      })
      |> ReasonReact.array)
    </form>
  </div>
  };

  <div id="entity">
    <button onClick=(_=>fetchEntity()) >(str("Fetch: " ++ resource.title ++ "/" ++ id))</button>
    <br/>
    (switch(state) {
      | NotAsked => (str("Not asked..."))
      | LoadFailure(msg) => (str("Load failure: " ++ msg))
      | Loading => (str("Loading..."))
      | LoadSuccess(entity) => switch(entity) {
          | Some(entity) => entity -> printEntity
          | None => str("No entity found")
      }
    })

  </div>
};