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

  React.useEffect1(
    () => {
      // useEffect1: will run when component is mounted, or when urlStack prop
      // is updated (even if it is the same route as the initial route)
      // - will trigger a refetch for both cases.
      fetchEntity(resource, entityId);
      Some(() => Js.log("cleanup Effect"));
    },
    [|urlStack|],
  );

  let showEntity = entity => {
    switch (urlStack) {
    | ["edit"] =>
      <EditView
        key={resource.name ++ "-edit-" ++ entityId}
        resource
        id=entityId
        entity
        refreshAction={_ =>
          // Note: pushing a route alone will not remount this component and
          // reset the state; but it will set a new urlStack prop. Therefore,
          // useEffect is used to fetch when either the component is mounted,
          // or when the urlStack prop is updated.
          ReasonReactRouter.push(
            "/" ++ resource.name ++ "/" ++ entityId,
          )
        }
      />
    | _ =>
      <div>
        <button
          onClick={_ =>
            ReasonReactRouter.push(
              "/" ++ resource.name ++ "/" ++ entityId ++ "/edit",
            )
          }>
          {str("Edit: " ++ resource.title ++ "/" ++ entityId)}
        </button>
        <DetailView
          key={resource.name ++ "-detail-" ++ entityId}
          resource
          id=entityId
          entity
        />
      </div>
    };
  };

  switch (entityState) {
  | NotAsked => str("Not asked...")
  | Loading => str("Loading entity: " ++ entityId ++ ", ...")
  | LoadFailure(msg) => str("Load Failure: " ++ msg)
  | LoadSuccess(entity) => showEntity(entity)
  };
};