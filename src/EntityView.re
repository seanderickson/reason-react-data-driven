open Belt;
open Common;
open Metadata;
open Store;

type editState =
  | Edit
  | View;

[@react.component]
let make =
    (
      ~resource: Resource.t,
      ~entityId: string,
      ~urlStack: list(string),
      ~initialState=NotAsked,
      ~refreshAction=_ => (),
      ~viewFunctionMap=(Js.Dict.empty(): Js.Dict.t(Js.Json.t => string)),
      ~children=ReasonReact.null,
      ~saveAction=?,
    ) => {
  Js.log3("initialState", entityId, initialState);
  let (entityState: webLoadingData(Js.Json.t), setEntityState) =
    React.useState(() => initialState);

  let (editState, setEditState) = React.useState(() => View);

  let fetchEntity = (resource: Resource.t, id) => {
    Js.log("fetch entity...");
    setEntityState(_ => Loading);
    switch (resource.name) {
    | "canonical" =>
      ApiClient.getRtEntity(resource.name, id)
      |> Js.Promise.then_(result => {
           switch (result) {
           | Result.Ok(entity) =>
             Js.log2("got rt entity: ", entity);
             setEntityState(_ => LoadSuccess(Belt.Option.getExn(entity[0])));
           | Result.Error(message) =>
             setEntityState(_ => LoadFailure(message))
           };
           Js.Promise.resolve();
         })
      |> ignore

    | _ =>
      ApiClient.getEntity(resource.name, id)
      |> Js.Promise.then_(result => {
           switch (result) {
           | Result.Ok(entity) => setEntityState(_ => LoadSuccess(entity))
           | Result.Error(message) =>
             setEntityState(_ => LoadFailure(message))
           };
           Js.Promise.resolve();
         })
      |> ignore
    };
  };

  React.useEffect1(
    () => {
      // useEffect1: will run when component is mounted, or when urlStack prop
      // is updated (even if it is the same route as the initial route)
      // - will trigger a refetch for both cases.
      // - this is required to refetch after an edit, ReasonReactRouter.push
      // does not remount.
      Js.log3("fetchEntity, stack", urlStack, List.length(urlStack));
      fetchEntity(resource, entityId);
      Some(() => Js.log("cleanup Effect"));
    },
    [|entityId|],
  );

  let showEntity = entity => {
    switch (editState) {
    | Edit =>
      <EntityEditFormView
        resource
        entity
        entityId
        refreshAction={_ => {
          // Note: pushing a route alone will not remount this component and
          // reset the state; but it will set a new urlStack prop. Therefore,
          // useEffect is used to fetch when either the component is mounted,
          // or when the urlStack prop is updated.
          // ReasonReactRouter.push("/" ++ resource.name ++ "/" ++ entityId)
          setEditState(_ => View);

          // NOTE: creates error:
          // Warning: Can't perform a React state update on an unmounted component
          // fetchEntity(resource, entityId);

          refreshAction();
        }}
        ?saveAction
      />
    | View =>
      <>
        // {printTitle(resource, entityId)}
        <h3>
          <button
            className="btn btn-gray"
            onClick={_ =>
              // ReasonReactRouter.push(
              //   "/" ++ resource.name ++ "/" ++ entityId ++ "/edit",
              // );
              setEditState(_ => Edit)}>
            {str("Edit: " ++ resource.title ++ "/" ++ entityId)}
          </button>
        </h3>
        <DetailView
          key={resource.name ++ "-detail-" ++ entityId}
          resource
          id=entityId
          entity
          viewFunctionMap>
          children
        </DetailView>
      </>
    };
  };
  switch (entityState) {
  | NotAsked => str("Not asked...")
  | Loading => str("Loading entity: " ++ entityId ++ ", ...")
  | LoadFailure(msg) => str("Load Failure: " ++ msg)
  | LoadSuccess(entity) => showEntity(entity)
  };
};