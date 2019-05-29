open Belt;
open Common;
open Store;

[@react.component]
let make = (~resource: resource, ~entityId: string, ~urlStack: list(string)) => {
  // Entity state

  let (entityState: webLoadingData(Js.Json.t), setEntityState) =
    React.useState(() => NotAsked);

  let fetchEntity = (resource, id) => {
    Js.log("fetch entity...");
    setEntityState(_ => Loading);
    ApiClient.getEntity(resource.name, id)
    |> Js.Promise.then_(result => {
         switch (result) {
         | Result.Ok(entity) => setEntityState(_ => LoadSuccess(entity))
         | Result.Error(message) => setEntityState(_ => LoadFailure(message))
         };
         Js.Promise.resolve();
       })
    |> ignore;
  };

  let showEntity = entity => {
    switch (urlStack) {
    | ["edit"] =>
      <EditView
        key={resource.name ++ ":" ++ entityId}
        resource
        id=entityId
        entity
        cancelAction={_ => {
          // Fixme: pushing a route is not forcing this componenent to reinitialize?
          // - so set the state back to NotAsked manually
          setEntityState(_ => NotAsked);

          ReasonReactRouter.push("/" ++ resource.name ++ "/" ++ entityId);
        }}
      />
    | _ =>
      <DetailView
        key={resource.name ++ ":" ++ entityId}
        resource
        id=entityId
        entity
      />
    };
  };

  switch (entityState) {
  | NotAsked =>
    fetchEntity(resource, entityId);
    str("Not asked...");
  | Loading => str("Loading entity: " ++ entityId ++ ", ...")
  | LoadFailure(msg) => str("Load Failure: " ++ msg)
  | LoadSuccess(entity) => showEntity(entity)
  };
};