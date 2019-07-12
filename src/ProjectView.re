open Belt;
open Common;
open Store;
open Metadata;
open EntityModules;

type addState =
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

  // Accessors for global context store data
  let {entityStoreState, fetchEntities, getEntity, getFilledResource}: EntityStore.ResourceContext.t =
    EntityStore.ResourceContext.useResources();

  // Entity state

  let (entityState, setEntityState) = React.useState(() => NotAsked);

  let (experimentState, setExperimentState) = React.useState(() => NotAsked);

  let (addState, setAddState) = React.useState(() => View);

  let addExperiment = currentExperiments => {
    // Create the default values for the form
    let defaults: Js.Dict.t(Js.Json.t) = Js.Dict.empty();

    Js.Dict.set(
      defaults,
      "project_id",
      projectId |> float_of_string |> Js.Json.number,
    );

    // Note: Prototype use case only: get the "next" experiment ID
    let newId =
      switch (currentExperiments) {
      | Some(exps) =>
        Belt.Array.reduce(exps, 1, (max, exp) =>
          Experiment.decode(exp)
          |> (expEntity => expEntity.id >= max ? expEntity.id + 1 : max)
        )
      | None => 1
      };
    Js.Dict.set(defaults, "id", newId |> float_of_int |> Js.Json.number);

    // Note: Prototype use case only: get the "next" experiment ordinal
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

    setAddState(_ => Add(string_of_int(newId), defaults |> Js.Json.object_));
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
    setEntityState(_ => Loading);
    ApiClient.getEntity("project", id)
    |> Js.Promise.then_(result => {
         switch (result) {
         | Result.Ok(entity) =>
           setEntityState(_ => {
             fetchExperiments();
             LoadSuccess(entity);
           })
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
    [|projectId|],
  );

  switch (entityState) {
  | NotAsked => str("Not asked...")
  | LoadFailure(msg) => str("Load failure: " ++ msg)
  | Loading => str("Loading project...")
  | LoadSuccess(entity) =>
    Js.log2("LoadSuccess", entity);
    let experimentResource =
      Belt.Option.getExn(getFilledResource("experiment"));
    // Create field display overrides using a "viewFunctionMap":
    // TODO: For demonstration only ( these examples can also be done using vocabulary)
    // TODO: demonstrate a similar "editFunctionMap" for custom editor implementations.
    let viewFunctionMap = Js.Dict.empty();

    let fieldName = "pi_id";
    Js.Dict.set(viewFunctionMap, fieldName, entity =>
      Resource.getField(resource, fieldName)
      |> Belt.Option.mapWithDefault(
           _, "field name not found: " ++ fieldName, field =>
           Metadata.fieldDecoder(~json=entity, ~field)
           |> (
             pi_id =>
               getEntity(`person, pi_id)
               |> Belt.Option.mapWithDefault(_, "no data for: " ++ pi_id, json =>
                    Person.decode(json)
                    |> (person => person.first_name ++ " " ++ person.last_name)
                  )
           )
         )
    );

    let fieldName = "primary_irb_id";
    Js.Dict.set(viewFunctionMap, fieldName, entity =>
      Resource.getField(resource, fieldName)
      |> Belt.Option.mapWithDefault(
           _, "field name not found: " ++ fieldName, field =>
           Metadata.fieldDecoder(~json=entity, ~field)
           |> (
             irb_id =>
               getEntity(`irb, irb_id)
               |> Belt.Option.mapWithDefault(
                    _, "no data for: " ++ irb_id, json =>
                    Irb.decode(json)
                    |> (irb => irb.irb_id ++ " - " ++ irb.type_)
                  )
           )
         )
    );

    <div>
      <EntityView
        resource
        entityId=projectId
        urlStack
        initialState={LoadSuccess(entity)}
        refreshAction={_ => {
          setAddState(_ => View);
          fetchProject(projectId);
        }}
        saveAction={entity => {
          // Call fetEntities on the store here: effectively a reset
          fetchEntities(`project);
          Js.log2("new entity recorded: ", entity);
        }}
        viewFunctionMap>
        {switch (addState) {
         | View =>
           switch (experimentState) {
           | NotAsked => str("Not asked...")
           | LoadFailure(msg) => str("Load failure: " ++ msg)
           | Loading => str("Loading experiments...")
           | LoadSuccess(entities) =>
             <div>
               <h3>
                 <button onClick={_ => addExperiment(entities)}>
                   {str("Add experiment")}
                 </button>
               </h3>
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
               setAddState(_ => View);
               fetchProject(projectId);
             }}
             saveAction={_ => {
               setAddState(_ => View);
               fetchProject(projectId);
               dispatchModal(
                 Show("Experiment saved: " ++ newId, _ => (), _ => ()),
               );
             }}
           />
         }}
      </EntityView>
    </div>;
  };
};