open Belt;
open Common;
open Store;

// Implement Entity models as functors

type project = {
  id: int,
  pi_id: int,
  primary_irb_id: option(int),
  entity: Js.Json.t,
};

module type EntityData = {
  let resource: resource;
  let entity: Js.Json.t;
};

module type EntityDataInt = {
  let resource: resource;
  let entity: Js.Json.t;
  let fields: Js.Dict.t(field);
  let getField: string => option(field);
  // let getFieldValue: string => option(string);
};

module Make_Entity = (EntityData: EntityData) : EntityDataInt => {
  let resource = EntityData.resource;
  let entity = EntityData.entity;

  let fields: Js.Dict.t(field) = Js.Dict.empty();
  Array.forEach(EntityData.resource.fields, field =>
    Js.Dict.set(fields, field.name, field)
  );

  let getField = name => Js.Dict.get(fields, name); // |> Belt.Option.getExn;

  // let getFieldValue = (name: string): option(string) =>
  //   getField(name)
  //   |> Belt.Option.flatMap(_, Decode.fieldDecodeOpt(EntityData.entity));
};

// module type ChannelDataInt = {
//   include EntityDataInt;
//   let getCycleOrdinal: unit => int;
//   let getAntibody: unit => antibody;
// };

// module type CycleDataInt = {
//   include EntityDataInt;
//   let getExperimentId: unit => int;
//   let getOrdinal: unit => int;
// };

module type ExperimentDataInt = {
  include EntityDataInt;
  let projectId: int;
  // let getProjectId: unit => int;
};

module Make_Experiment = (EntityData: EntityData) : ExperimentDataInt => {
  include Make_Entity(EntityData);

  let projectId =
    entity
    |> Json.Decode.(
         field(Belt.Option.getExn(getField("project_id")).name, int)
       );
  // let projectField = Belt.Option.getExn(getField("project_id"));
  // // let getProjectId = () => getField("project_id")
  // //   |> Belt.Option.map(_, schemaField => entity |> Json.Decode.(field(schemaField.name, int)))
  // //   |> Belt.Option.getExn;
  // let getProjectId = () => entity |> Json.Decode.(field(projectField.name, int));
  // let getProjectId = () => entity |> Json.Decode.(field("project_id", int));
};

module type ProjectDataInt = {
  include EntityDataInt;
  let getId: unit => int;
  let getExperiments:
    (array(Js.Json.t), resource) => array(module ExperimentDataInt);
};

module Make_Project = (EntityData: EntityData) : ProjectDataInt => {
  include Make_Entity(EntityData);

  let getId = () => entity |> Json.Decode.(field("id", int));

  let getExperiments = (allExperiments, expResource) => {
    allExperiments
    |> Belt.Array.keepMap(
         _,
         exp => {
           module ExpModule =
             Make_Experiment({
               let resource = expResource;
               let entity = exp;
             });
           if (ExpModule.projectId == getId()) {
             // Note: Convert into a "first-class" packaged module,
             // so that it can be passed around.
             Some(
               (module ExpModule): (module ExperimentDataInt),
             );
           } else {
             None;
           };
         },
       );
  };
};