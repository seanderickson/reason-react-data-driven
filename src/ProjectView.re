open Belt;
open Common;
open Store;
open Metadata;
open EntityModules;

type newState =
  | Add(string, Js.Json.t)
  | View;

[@react.component]
let make =
    (
      ~resource: Resource.t,
      ~projectId: string,
      ~dispatchModal,
      ~urlStack: list(string),
    ) => {
  Js.log3("show project: ", projectId, urlStack);
  let {resourceState, fetchResources, getResource}: Store.ResourceContext.t =
    Store.ResourceContext.useResources();
  // Entity state

  let (entityState, setEntityState) = React.useState(() => NotAsked);

  let (experimentState, setExperimentState) = React.useState(() => NotAsked);

  let (expState, setExpState) = React.useState(() => View);

  let addExperiment = currentExperiments => {
    let defaults: Js.Dict.t(Js.Json.t) = Js.Dict.empty();

    Js.Dict.set(
      defaults,
      "project_id",
      projectId |> float_of_string |> Js.Json.number,
    );

    // Prototype use case: get the "next" experiment ID
    let newId =
      switch (currentExperiments) {
      | Some(exps) =>
        Belt.Array.reduce(
          exps,
          1,
          (max, exp) => {
            let expEntity = Experiment.decode(exp);
            expEntity.id >= max ? expEntity.id + 1 : max;
          },
        )
      | None => 1
      };
    Js.Dict.set(defaults, "id", newId |> float_of_int |> Js.Json.number);

    // Prototype use case: get the "next" experiment ordinal
    let newOrdinal =
      switch (
        currentExperiments
        |> Experiment.filterExperiments(projectId |> int_of_string)
      ) {
      | Some(exps) =>
        Belt.Array.reduce(
          exps,
          1,
          (max, exp) => {
            let expEntity = Experiment.decode(exp);
            expEntity.number >= max ? expEntity.number + 1 : max;
          },
        )
      | None => 1
      };
    Js.Dict.set(
      defaults,
      "number",
      newOrdinal |> float_of_int |> Js.Json.number,
    );

    // TODO: set the current date

    setExpState(_ => Add(string_of_int(newId), defaults |> Js.Json.object_));
  };

  let fetchExperiments = () => {
    // NOTE: getting all experiments here:
    // - so that the "next" experiment ID may be calculated (prototype use case)
    Js.log("fetch experiments...");
    setExperimentState(_ => Loading);
    ApiClient.getEntityListing("experiment")
    |> Js.Promise.then_(result => {
         switch (result) {
         | Result.Ok(entities) =>
           setExperimentState(_ => LoadSuccess(Some(entities)))
         | Result.Error(message) =>
           setExperimentState(_ => LoadFailure(message))
         };
         Js.Promise.resolve();
       })
    |> ignore;
  };

  let fetchProject = id => {
    Js.log("fetch project...");
    setEntityState(_ => Loading);
    ApiClient.getEntity("project", id)
    |> Js.Promise.then_(result => {
         switch (result) {
         | Result.Ok(entity) =>
           setEntityState(_ => {
             fetchExperiments();
             LoadSuccess(entity);
           })
         //  fetchExperiments();
         | Result.Error(message) => setEntityState(_ => LoadFailure(message))
         };
         Js.Promise.resolve();
       })
    |> ignore;
  };

  React.useEffect1(
    () => {
      fetchProject(projectId);
      Some(() => Js.log("cleanup Effect"));
    },
    [|urlStack|],
  );

  switch (entityState) {
  | NotAsked => str("Not asked...")
  | LoadFailure(msg) => str("Load failure: " ++ msg)
  | Loading => str("Loading project...")
  | LoadSuccess(entity) =>
    let experimentResource = Belt.Option.getExn(getResource("experiment"));
    <div>
      <EntityView
        resource
        entityId=projectId
        urlStack
        initialState={LoadSuccess(entity)}
      />
      <h3> {str("Experiments")} </h3>
      {switch (expState) {
       | View =>
         switch (experimentState) {
         | NotAsked => str("Not asked...")
         | LoadFailure(msg) => str("Load failure: " ++ msg)
         | Loading => str("Loading experiments...")
         | LoadSuccess(entities) =>
           // FIXME: Add button should not show if the project is being edited
           <div>
             <button onClick={_ => addExperiment(entities)}>
               {str("Add")}
             </button>
             <EntityListingTable
               resource=experimentResource
               initialState={
                 LoadSuccess(
                   Experiment.filterExperiments(
                     projectId |> int_of_string,
                     entities,
                   ),
                 )
               }
             />
           </div>
         }
       | Add(newId, newEntity) =>
         <EditView
           key={experimentResource.name ++ "-edit-" ++ projectId}
           resource=experimentResource
           id=newId
           entity=newEntity
           isNew=true
           refreshAction={_ => {
             setExpState(_ => View);
             fetchProject(projectId);
           }}
           saveAction={_ => {
             setExpState(_ => View);
             fetchProject(projectId);
             dispatchModal(
               Show("Experiment saved: " ++ newId, _ => (), _ => ()),
             );
           }}
         />
       }}
    </div>;
  };
};