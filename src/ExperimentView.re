open Belt;
open Common;
open Store;
open Metadata;
open EntityModules;

[@react.component]
let make =
    (
      ~resource: Resource.t,
      ~entityId: string,
      ~dispatchModal,
      ~urlStack: list(string),
    ) => {
  // let {resourceState, fetchResources, getResource}: Store.ResourceContext.t =
  //   Store.ResourceContext.useResources();
  // let {reducerState, fetchEntities, getEntity, getUserVocabulary}: EntityStore.ResourceContext.t =
  //   EntityStore.ResourceContext.useResources();

  // Entity state

  let (entityState, setEntityState) = React.useState(() => NotAsked);

  let fetchExperiment = id => {
    Js.log("fetch project...");
    setEntityState(_ => Loading);
    ApiClient.getEntity("experiment", id)
    |> Js.Promise.then_(result => {
         switch (result) {
         | Result.Ok(entity) => setEntityState(_ => LoadSuccess(entity))
         | Result.Error(message) => setEntityState(_ => LoadFailure(message))
         };
         Js.Promise.resolve();
       })
    |> ignore;
  };

  React.useEffect1(
    () => {
      fetchExperiment(entityId);
      Some(() => Js.log("cleanup Effect"));
    },
    [|urlStack|],
  );

  switch (entityState) {
  | NotAsked => str("Not asked...")
  | LoadFailure(msg) => str("Load failure: " ++ msg)
  | Loading => str("Loading project...")
  | LoadSuccess(entity) =>
    <div>
      <EntityView
        resource
        entityId
        urlStack
        initialState={LoadSuccess(entity)}
        refreshAction={_ =>
          // setAddState(_ => View);
          fetchExperiment(entityId)}
      />
    </div>
  };
};