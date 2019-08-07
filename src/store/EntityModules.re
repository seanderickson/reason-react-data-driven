let nullDecoder = json => json;

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
    objectives: array(string),
    filters: array(string),
  };

  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      name: json |> field("name", string),
      objectives: json |> field("objectives", array(string)),
      filters: json |> field("filters", array(string)),
    };

  let getId = json => decode(json).id |> string_of_int;
};

module Person = {
  type t = {
    id: int,
    first_name: string,
    last_name: string,
    lab_id: int,
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
    type_: string,
  };

  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      irb_id: json |> field("irb_id", string),
      lab_id: json |> field("id", int),
      type_: json |> field("type", string),
    };

  let getId = json => decode(json).id |> string_of_int;
};

module Cycle = {
  type t = {
    id: int,
    ordinal: int,
    experiment_id: int,
  };

  let decode = (json: Js.Json.t): t =>
    Json.Decode.{
      id: json |> field("id", int),
      ordinal: json |> field("id", int),
      experiment_id: json |> field("experiment_id", int),
    };

  let getId = json => decode(json).id |> string_of_int;
};

module CycleChannel = {
  type t = {
    id: int,
    cycle_id: int,
    channel_id: int,
  };

  let decode = (json: Js.Json.t): t => {
    // Js.log2("decode CycleChannel" , json);
    Json.Decode.{
      id: json |> field("id", int),
      cycle_id: json |> field("cycle_id", int),
      channel_id: json |> field("channel_id", int),
    };
  };
  let getId = json => decode(json).id |> string_of_int;

  let belongsTo = (~entity: Js.Json.t, ~cycle: Js.Json.t) =>
    decode(entity).cycle_id == Cycle.decode(cycle).id;

  let updateChannels = (updatedChannel, channels) =>
    Belt.Array.map(channels, channelEntity =>
      getId(channelEntity) == getId(updatedChannel)
        ? updatedChannel : channelEntity
    );

  let updateErrors = (errors, channel, newError) => {
    Js.log4(
      "add to errors: ",
      errors,
      getId(channel),
      Belt.Map.String.has(errors, getId(channel)),
    );
    let newMap =
      Belt.Map.String.has(errors, getId(channel))
        ? Belt.Map.String.mapWithKey(errors, (channelId, error) =>
            getId(channel) == channelId ? newError : error
          )
        : Belt.Map.String.set(errors, getId(channel), newError);
    Js.log4(
      "add to errors: ",
      newMap,
      getId(channel),
      Belt.Map.String.has(errors, getId(channel)),
    );
    newMap;
  };
};

module ReagentData = {
  module Batch = {
    type t = {id: int};

    let decode = json => Json.Decode.{id: json |> field("id", int)};
  };
  module Canonical = {
    type t = {
      href: string,
      restricted: bool,
      batches: option(array(Batch.t)),
    };

    let decode = json =>
      Json.Decode.{
        href: json |> field("href", string),
        restricted: json |> field("restricted", bool),
        batches: json |> optional(field("batches", array(Batch.decode))),
      };
  };

  type t = {canonicals: option(array(Canonical.t))};

  let decode = json =>
    Json.Decode.{
      canonicals:
        json |> optional(field("canonicals", array(Canonical.decode))),
    };

  let jsonArrayDecoder =
    Json.Decode.field("canonicals", Json.Decode.array(nullDecoder));
};