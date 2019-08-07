open Belt;
open Common;
open Store;
open Metadata;
open EntityModules;

[@react.component]
let make =
    (
      ~resource: Resource.t, // NOTE: should be tied to the Experiment Resource
      ~entityId: string,
      ~dispatchModal,
      ~urlStack: list(string),
    ) => {
  let {entityStoreState, fetchEntities, getEntity, getFilledResource}: EntityStore.ResourceContext.t =
    EntityStore.ResourceContext.useResources();

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
  let printTabs = () => {
    let testUrl = (urlStack, test) =>
      switch (urlStack) {
      | [testval, ..._] => testval == test
      | _ => Js.String.length(test) == 0 ? true : false
      };

    let getListItemClass = (urlStack, test) => {
      testUrl(urlStack, test) ? "-mb-px mr-1" : "mr-1";
    };
    let getItemLinkClass = (urlStack, test) =>
      if (test == "actual") {
        "bg-white inline-block py-2 px-4 text-gray-400 font-semibold"; // 'inactive'
      } else {
        testUrl(urlStack, test)
          ? "bg-white inline-block border-l border-t border-r rounded-t py-2 px-4 text-blue-700 font-semibold"
          : "bg-white inline-block py-2 px-4 text-blue-500 hover:text-blue-800 font-semibold";
      };
    let handleClick = (href, event) => {
      ReactEvent.Mouse.preventDefault(event);
      Js.log2("handleClick", href);
      ReasonReactRouter.push(
        "/" ++ resource.name ++ "/" ++ entityId ++ "/" ++ href,
      );
    };

    <ul key="exp_options" className="flex border-b">
      <li key="main" className={getListItemClass(urlStack, "")}>
        <a
          className={getItemLinkClass(urlStack, "")}
          onClick={handleClick("")}
          href={"/experiment/" ++ entityId}>
          {str("Overview")}
        </a>
      </li>
      <li
        key="acquisitions"
        className={getListItemClass(urlStack, "acquisitions")}>
        <a
          className={getItemLinkClass(urlStack, "acquisitions")}
          onClick={handleClick("acquisitions")}
          href="acquisitions">
          {str("Acquisitions")}
        </a>
      </li>
      <li key="planned" className={getListItemClass(urlStack, "planned")}>
        <a
          className={getItemLinkClass(urlStack, "planned")}
          onClick={handleClick("planned")}
          href="planned">
          {str("Planned Cycles")}
        </a>
      </li>
      <li key="actual" className={getListItemClass(urlStack, "actual")}>
        <a
          className={getItemLinkClass(urlStack, "actual")}
          onClick={handleClick("actual")}
          href="actual">
          {str("Actual Cycles")}
        </a>
      </li>
    </ul>;
  };
  let printMain = entity =>
    <EntityView
      resource
      entityId
      urlStack
      initialState={LoadSuccess(entity)}
      refreshAction={_ => fetchExperiment(entityId)}
    />;

  switch (entityState) {
  | NotAsked => str("Not asked...")
  | LoadFailure(msg) => str("Load failure: " ++ msg)
  | Loading => str("Loading project...")
  | LoadSuccess(entity) =>
    <>
      {printTabs()}
      {switch (urlStack) {
       | [subpath, ..._] =>
         switch (subpath) {
         | "acquisitions" => str("Microscope / Acquisitions")
         | "planned" =>
           <CycleView
             experimentId={int_of_string(entityId)}
             cycleResource={getFilledResource("cycle") |> Belt.Option.getExn}
             cycleChannelResource={
               getFilledResource("cycle_channel") |> Belt.Option.getExn
             }
           />
         | "actual" => str("Actual table")
         | _ => printMain(entity)
         }
       | _ => printMain(entity)
       }}
    </>
  };
};