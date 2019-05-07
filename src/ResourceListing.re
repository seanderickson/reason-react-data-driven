open Common;
open Store;

[@react.component]
let make = ()=> {

  let {resourceState, fetchResources, setResources}: Store.ResourceContext.t = Store.ResourceContext.useResources();

  let printResources = resourceOption => {
    switch(resourceOption){
      | Some(resources) => 
        resources
        |> Array.map( resource => {
          <div key=(string_of_int(resource.id)) className="channel_row" >
            <div className="channel_item">(str(string_of_int(resource.id)))</div>
            <div className="channel_item">(str(resource.name))</div>
          </div>
        })
        |> ReasonReact.array;
      | None => ReasonReact.null
    }
  };

  <div id="resources">
    <button onClick=(_=>fetchResources()) >(str("Fetch Resources..."))</button>
    <br/>
    <h3>(str("Resources:"))</h3>
    <form className="channel_form" id="resources" >
      <div className="channel_row" >
        <label className="channel_item" >(str("Id"))</label><label className="channel_item" >(str("Name"))</label>
      </div>
      (printResources(resourceState.resources))
    </form>
  </div>
};