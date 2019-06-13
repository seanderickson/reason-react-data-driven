open Belt;
open Common;

module type EntityData = {
  type t;
  let decode: Js.Json.t => t;
};

module Project = {
  type t = {
    id: int,
    pi_id: int,
    primary_irb_id: option(int),
  };
  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      pi_id: json |> field("pi_id", int),
      primary_irb_id: json |> optional(field("primary_irb_id", int)),
    };
};

module Experiment = {
  type t = {
    id: int,
    project_id: int,
    number: int,
  };

  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      project_id: json |> field("project_id", int),
      number: json |> field("number", int),
    };

  let filterExperiments = (projectId, entities) => {
    let experiments =
      Belt.Option.map(entities, exps =>
        Belt.Array.keep(
          exps,
          exp => {
            // let projectEntity = Project.decode(projectEntity);
            let expEntity = decode(exp);
            projectId == expEntity.project_id;
          },
        )
      );
    experiments;
  };
};

// A functor to extend the EntityData interface
module Make_EntityData(D: EntityData): EntityData with type t=D.t = {
  include D;
};

module E2 = Make_EntityData({
  type t = {
    id: int,
    project_id: int,
    number: int,
  };

  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      project_id: json |> field("project_id", int),
      number: json |> field("number", int),
    };

  let filterExperiments = (projectId, entities) => {
    let experiments =
      Belt.Option.map(entities, exps =>
        Belt.Array.keep(
          exps,
          exp => {
            // let projectEntity = Project.decode(projectEntity);
            let expEntity = decode(exp);
            projectId == expEntity.project_id;
          },
        )
      );
    experiments;
  };

})