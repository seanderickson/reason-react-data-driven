open Belt;
open Common;
open Store;
open EntityModules;

module Experiments = {
  type state = webLoadingData(option(array(Js.Json.t)));
  // type state = webLoadingData(option(array(ExperimentDecoder)));

  [@react.component]
  let make = (~resource, ~projData, ~urlStack: list(string)) => {
    let (state, setState) = React.useState(() => NotAsked);
    let {resourceState, fetchResources, getResource}: Store.ResourceContext.t =
      Store.ResourceContext.useResources();

    let fetchExperiments = projData => {
      Js.log("fetch experiments...");
      // unpack the "first class" module
      module PD = (val projData: ProjectDataInt);
      Js.log("setState Loading...");
      setState(_ => Loading);
      ApiClient.getEntityListing("experiment")
      |> Js.Promise.then_(result => {
           switch (result) {
           | Result.Ok(entities) =>
             Js.log2("got experiments", entities);
             let experiments =
               entities
               |> Belt.Array.keep(
                    _,
                    exp => {
                      module ED =
                        Make_Experiment({
                          let resource = resource;
                          let entity = exp;
                        });
                      // PD.getId()==ED.getProjectId();
                      PD.getId() == ED.projectId;
                    },
                  );
             Js.log("setState exp LoadSuccess...");
             setState(_ => LoadSuccess(Some(experiments)));
           | Result.Error(message) => setState(_ => LoadFailure(message))
           };
           Js.Promise.resolve();
         })
      |> ignore;
    };

    React.useEffect1(
      () =>
        {
          // useEffect1: will run when component is mounted, or when urlStack prop
          // is updated (even if it is the same route as the initial route)
          // - will trigger a refetch for both cases.
          fetchExperiments(projData);
          Some(() => ());
        },
        // Some(() => Js.log("cleanup Effect"));
      [|urlStack|],
    );

    switch (state) {
    | NotAsked => str("Not asked...")
    | LoadFailure(msg) => str("Load failure: " ++ msg)
    | Loading => str("Loading...")
    | LoadSuccess(entities) =>
      let experimentResource = Belt.Option.getExn(getResource("experiment"));
      <EntityListingTable
        resource=experimentResource
        initialState={LoadSuccess(entities)}
      />;
    };
  };
};

[@react.component]
let make = (~resource, ~entityId: string, ~urlStack: list(string)) => {
  Js.log3("show project: ", entityId, urlStack);
  let {resourceState, fetchResources, getResource}: Store.ResourceContext.t =
    Store.ResourceContext.useResources();
  // Entity state

  let (entityState: webLoadingData(module ProjectDataInt), setEntityState) =
    React.useState(() => NotAsked);

  let fetchProject = id => {
    Js.log("fetch project...");
    setEntityState(_ => Loading);
    ApiClient.getEntity(resource.name, id)
    |> Js.Promise.then_(result => {
         switch (result) {
         | Result.Ok(entity) =>
           module ProjectModule =
             Make_Project({
               let resource = resource;
               let entity = entity;
             });
           Js.log("setState exp project LoadSuccess...");
           setEntityState(_ => LoadSuccess((module ProjectModule)));
         | Result.Error(message) => setEntityState(_ => LoadFailure(message))
         };
         Js.Promise.resolve();
       })
    |> ignore;
  };

  React.useEffect1(
    () => {
      fetchProject(entityId);
      Some(() => Js.log("cleanup Effect"));
    },
    [||],
  );

  switch (entityState) {
  | NotAsked => str("Not asked...")
  | LoadFailure(msg) => str("Load failure: " ++ msg)
  | Loading => str("Loading proj...")
  | LoadSuccess(projectEntity) =>
    module PD = (val projectEntity: ProjectDataInt);
    let experimentResource = Belt.Option.getExn(getResource("experiment"));
    <div>
      <EntityView
        resource
        entityId
        urlStack
        initialState={LoadSuccess(PD.entity)}
      />
      <h3> {str("Experiments")} </h3>
      <Experiments
        resource=experimentResource
        projData=projectEntity
        urlStack
      />
    </div>;
  };
};