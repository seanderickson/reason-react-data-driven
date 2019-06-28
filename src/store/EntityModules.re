
module type EntityData = {
  type t;
  let decode: Js.Json.t => t;
  let getId: Js.Json.t => string;
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

  let getId = json => decode(json).id |> string_of_int;
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

  let getId = json => decode(json).id |> string_of_int;

  let filterExperiments = (projectId, entities) => {
    let experiments =
      Belt.Option.map(entities, exps =>
        Belt.Array.keep(
          exps,
          exp => {
            let expEntity = decode(exp);
            projectId == expEntity.project_id;
          },
        )
      );
    experiments;
  };
};

module Microscope = {
  type t = {
    id: int,
    name: string,
    magnification: int,
  };

  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      name: json |> field("name", string),
      magnification: json |> field("magnification", int),
    };

  let getId = json => decode(json).id |> string_of_int;
};

module Person = {
  type t = {
    id: int,
    first_name: string,
    last_name: string,
    lab_id: int
  };

  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      first_name: json |> field("first_name", string),
      last_name: json |> field("last_name", string),
      lab_id: json |> field("id", int),
    };

  let getId = json => decode(json).id |> string_of_int;
};

module Irb = {
  type t = {
    id: int,
    irb_id: string,
    lab_id: int,
    type_: string
  };

  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      irb_id: json |> field("irb_id", string),
      lab_id: json |> field("id", int),
      type_: json |> field("type", string)
    };

  let getId = json => decode(json).id |> string_of_int;
};

// module type EntityDataInt = {
//   type t;
//   let decode: Js.Json.t => t;
//   let decodeMany: Js.Json.t => array(t);
// };

// module Make_Entity =
//        (Input: EntityData)
//        : (EntityDataInt with type t = Input.t) => {
//   include Input;
//   let decodeMany = (json: Js.Json.t): array(t) =>
//     Json.Decode.(json |> array(decode));
// };

// module Experiment2 = Make_Entity(Experiment);
// module Project2 = Make_Entity(Project) /*     experiments*/ /*       )*/;

/// testing

// module type EntityDataInt = {
//   type t;
//   let decode: Js.Json.t => t;
//   // let filterExperiments: (int, option(array(Js.Json.t)))=>option(array(Js.Json.t));
// };

// // A functor to extend the EntityData interface
// module Make_EntityData(D: EntityData): EntityDataInt with type t=D.t = {
//   include D;
// };

// module type ExpInput = {
//   type t = {
//     id: int,
//     project_id: int,
//     number: int,
//   };

// };

// module Experiment2 = Make_EntityData({

//   let decode = (json: Js.Json.t): t =>
//     Json.Decode.{
//       id: json |> field("id", int),
//       project_id: json |> field("project_id", int),
//       number: json |> field("number", int),
//     };

//   let filterExperiments = (projectId, entities) => {
//     let experiments =
//       Belt.Option.map(entities, exps =>
//         Belt.Array.keep(
//           exps,
//           exp => {
//             // let projectEntity = Project.decode(projectEntity);
//             let expEntity = decode(exp);
//             projectId == expEntity.project_id;
//           },
//         )