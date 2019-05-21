open Common;
open Store;

[@react.component]
let make = ()=> {

  let {resourceState, fetchResources, setResources}: Store.ResourceContext.t = Store.ResourceContext.useResources();

  let printResources = resources => {
    
    <div>
    <h3>(str("Resources:"))</h3>
    <form className="grid_table" id="resources" >
      <div className="grid_table_row grid_table_header" >
        <label className="font-bold" >(str("Id"))</label>
        <label className="font-bold" >(str("Name"))</label>
        <label className="font-bold" >(str("Title"))</label>
        <label className="font-bold" >(str("Description"))</label>
        <label className="font-bold" >(str("Fields"))</label>
      </div>
    (resources
    |> Array.map( resource => {
      <div key=(string_of_int(resource.id)) className="grid_table_row" >
        <div className="grid_table_row_cell">(str(string_of_int(resource.id)))</div>
        <div className="grid_table_row_cell">(str(resource.name))</div>
        <div className="grid_table_row_cell">(str(resource.title))</div>
        <div className="grid_table_row_cell">(str(resource.description))</div>
        <div className="grid_table_row_cell">(
          resource.fields
          |> Array.map((field:field)=>field.name)
          |> Js.Array.joinWith(", ")
          |> str
          )</div>
      </div>
    })
    |> ReasonReact.array)
    </form>
  </div>
  };

  <div id="resources">
    <button onClick=(_=>fetchResources()) >(str("Fetch Resources..."))</button>
    <br/>
    (switch(resourceState) {
      | NotAsked => (str("Not asked..."))
      | LoadFailure(msg) => (str("Load failure: " ++ msg))
      | Loading => (str("Loading..."))
      | LoadSuccess(resources) => switch(resources) {
          | Some(resources) => resources -> printResources
          | None => str("No resources found")
      }
    })

  </div>
};