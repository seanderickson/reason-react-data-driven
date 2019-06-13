open Belt;
open Common;
open Metadata;
open Store;

type state = webLoadingData(option(array(Js.Json.t)));
// let initial:state = NotAsked;

[@react.component]
let make = (~resource: Resource.t, ~initialState=NotAsked, ()) => {
  let (state, setState) = React.useState(() => initialState);

  let fetchEntities = () => {
    Js.log("fetchEntities...")
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
      if (state==NotAsked){
        fetchEntities();
      }
      Some(() => Js.log("cleanup Effect"));
    },
    [|resource.name|],
  );

  let handleRoute = (endpoint, id, evt) => {
    ReactEvent.Synthetic.preventDefault(evt);
    ReasonReactRouter.push("/" ++ Js.Array.joinWith("/", [|endpoint, id|]));
  };

  let printEntities = entities => {
    <div>
      <h3 className="shadow">
        {str(resource.title ++ " listing")}
      </h3>
      <table
        className="table-fixed min-w-full border-separate" id="entity_table">
        <thead>
          <tr key="header" className="">
            {resource.fields
             |> Array.map(_, field =>
                  <th key={field.name} className="font-bold text-left">
                    {str(field.title)}
                  </th>
                )
             |> ReasonReact.array}
          </tr>
        </thead>
        <tbody>
          {entities
           |> Array.mapWithIndex(_, (i, entity) =>
                // let fieldReader = Store.Decode.fieldDecoder(entity, resource);
                <tr key={string_of_int(i)} className="hover:bg-grey-lighter">
                  {resource.fields
                   |> Array.map(
                        _,
                        field => {
                          // let fvalue = fieldReader(field.name);
                          let fvalue =
                            Metadata.fieldDecoder(entity, field);
                          <td key={field.name} className="text-left">
                            {switch (field.ref_endpoint) {
                             | Some(endpoint) =>
                               <a
                                 onClick={handleRoute(endpoint, fvalue)}
                                 href={"/" ++ endpoint ++ "/" ++ fvalue}>
                                 {str(fvalue)}
                               </a>
                             | None => str(fvalue)
                             }}
                          </td>;
                        },
                      )
                   |> ReasonReact.array}
                </tr>
              )
           |> ReasonReact.array}
        </tbody>
      </table>
    </div>;
  };

  <div id="entities">
    <button onClick={_ => fetchEntities()}>
      {str("Fetch: " ++ resource.title)}
    </button>
    <br />
    {
      // TODO: move the entity fetch logic out of this component
      switch (state) {
     | NotAsked => str("Not asked...")
     | LoadFailure(msg) => str("Load failure: " ++ msg)
     | Loading => str("Loading...")
     | LoadSuccess(entities) =>
       switch (entities) {
       | Some(entities) => entities->printEntities
       | None => str("No entities found")
       }
     }}
  </div>;
};