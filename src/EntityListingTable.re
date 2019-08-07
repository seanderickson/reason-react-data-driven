
open Belt;
open Common;
open Metadata;
open Store;

type state = webLoadingData(option(array(Js.Json.t)));

module EntityTable = {
  let printFieldHeaderCell = (field: Field.t): ReasonReact.reactElement =>
    <th key={field.name} className="cell cell-header">
      {str(field.title)}
    </th>;

  let printFieldCell = (entity, field: Field.t) => {
    <td key={field.name} className="cell text-left">
      {DetailView.EntityDetail.getFieldDisplayValue(entity, field)}
    </td>;
  };

  let printTableTitle = (resource: Resource.t) =>
    <div
      className="border-solid border-2 border-gray-300 text-center rounded m-2">
      {str(resource.title ++ " listing")}
    </div>;

  let printTable = (resource: Resource.t, entities) => {
    // <div className="data-table-container">
    // <div className="data-table-scroll-container">
    <table className="data-table" id="entity_table">

        <thead>
          <tr key="header" className="">
            {Array.map(resource.fields, field => printFieldHeaderCell(field))
             |> ReasonReact.array}
          </tr>
        </thead>
        <tbody>
          {entities
           |> Array.mapWithIndex(_, (i, entity) =>
                <tr key={string_of_int(i)} className="hover:bg-grey-lighter">
                  {resource.fields
                   |> Array.map(_, field => printFieldCell(entity, field))
                   |> ReasonReact.array}
                </tr>
              )
           |> ReasonReact.array}
        </tbody>
      </table>;
      // </div>
      // </div>;
  };
};

[@react.component]
let make = (~resource: Resource.t, ~initialState=NotAsked, ()) => {
  let (state, setState) = React.useState(() => initialState);

  let fetchEntities = () => {
    Js.log("fetchEntities...");
    setState(_ => Loading);
    ApiClient.getEntityListing(resource.name)
    |> Js.Promise.then_(result => {
         switch (result) {
         | Result.Ok(entities) => setState(_ => LoadSuccess(Some(entities)))
         | Result.Error(message) => setState(_ => LoadFailure(message))
         };
         Js.Promise.resolve();
       })
    |> ignore;
  };

  React.useEffect1(
    () => {
      if (state == NotAsked) {
        fetchEntities();
      };
      Some(() => Js.log("cleanup Effect"));
    },
    [|resource.name|],
  );

  <div id="entities">
    {switch (state) {
     | NotAsked => str("Not asked...")
     | LoadFailure(msg) => str("Load failure: " ++ msg)
     | Loading => str("Loading...")
     | LoadSuccess(entities) =>
       switch (entities) {
       | Some(entities) =>
         <div>
           {EntityTable.printTableTitle(resource)}
           {EntityTable.printTable(resource, entities)}
         </div>
       | None => str("No entities found")
       }
     }}
  </div>;
};