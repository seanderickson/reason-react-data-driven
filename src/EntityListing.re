open Belt;
open Common;
open Store;


type state = {
  entityListing: option(array(Js.Json.t)),
  error: option(string),
  isLoading: bool
};

let initialState = {
  entityListing: None, error: None, isLoading: true
};

[@react.component]
let make = (~resource: resource )=> {

  let (state, setState) = React.useState(() => initialState);

  let fetchEntities = () => {
    ApiClient.getEntityListing(resource.name)
    |> Js.Promise.then_(result => {
      switch(result) {
        | Result.Ok(entities) => setState(_ => {...initialState, entityListing: Some(entities) })
        | Result.Error(message) => setState(_=>{...initialState, error: Some(message)})
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
          <div key=field.name className="grid_table_row_cell">(str(fieldReader(field.name)))</div>
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
    (switch(state.entityListing) {
      | Some(entities) => entities |> printEntities
      | None => ReasonReact.null
    })

    (switch(state.error) {
      | Some(error) => {
        <div className="error" >(str(error))</div>
      }
      | None => ReasonReact.null
    })


  </div>
};