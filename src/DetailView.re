open Belt;
open Common;
open Store;

type loadState('a) = 
  | NotAsked
  | Loading
  | LoadFailure(string)
  | LoadSuccess('a);


type viewState = 
  | View
  | Edit;

// type state = webLoadingData(option(Js.Json.t));
type state = loadState(Js.Json.t);
let initialState = NotAsked;


[@react.component]
let make = (~resource: resource, ~id: string, ~viewState=View, ~initialEntity=?, () )=> {

  let (state, setState) = React.useState(() => Belt.Option.getWithDefault(initialEntity, initialState));

  let fetchEntity = () => {
    Js.log("fetch entity...")
    setState(_=>Loading);
    ApiClient.getEntity(resource.name, id)
    |> Js.Promise.then_(result => {
      switch(result) {
        | Result.Ok(entity) => {
          setState(_ => LoadSuccess(entity))
        }
        | Result.Error(message) => setState(_=>LoadFailure(message))
        };
        Js.Promise.resolve();
      })
    |> ignore;
  };

  React.useEffect1(() => {
    if (state == NotAsked) fetchEntity();
    Some(() => {
      Js.log("cleanup Effect detailview");
    });
  }, [|resource.name|]);


  let printRow = (field: field, rvalue) => {
    <div key=("row-field-" ++ field.name) className="detail_table_row">
      <div className="md:text-right">
        <label className="font-bold text-right" htmlFor=("inline-" ++ field.name) >
          (str(field.title ++ ": "))
        </label>
      </div>
      <div className="">
        <span id=("inline-" ++ field.name) className="text-left">
          rvalue
        </span>
      </div>
    </div>
  };


  let printEntity = entity => {
    // let fieldReader = Store.Decode.fieldDecoder(entity, resource);
    
    <div>
      <button onClick=(_=>{
        ReasonReactRouter.push("/" ++ resource.name ++ "/" ++ id ++ "/edit");
        }) >
        (str("Edit: " ++ resource.title ++ "/" ++ id))
      </button>
      <h3 className="shadow" >(str("Entity detail: " ++ resource.title ++ ": " ++ id))</h3>
      <form className="detail_table" id="entity_table" >

      (resource.fields
      |> Array.map(_, field => {
        // let fvalue = fieldReader(field.name);
        let fvalue = Store.Decode.fieldDecoder(entity, field);
        printRow(field,
          (switch(field.ref_endpoint) {
            | Some(endpoint) =>
              <Link href=("/" ++ endpoint ++ "/" ++ fvalue) selected=false >(str(fvalue))</Link>
            | None => (str(fvalue))
          }))
      })
      |> ReasonReact.array)
      </form>
    </div>
  };

  <div id="entity">
    <button onClick=(_=>fetchEntity()) >(str("Fetch: " ++ resource.title ++ "/" ++ id))</button>

    (switch(state) {
      | NotAsked => (str("Not asked..."))
      | LoadFailure(msg) => (str("Load failure: " ++ msg))
      | Loading => (str("Loading..."))
      | LoadSuccess(entity) => {
        switch(viewState) {
          | View => entity -> printEntity
          | Edit => 
            <EditView resource=resource entity=entity 
              cancelAction=(_=>ReasonReactRouter.push("/" ++ resource.name ++ "/" ++ id)) />
        }
      }
    })

  </div>
};