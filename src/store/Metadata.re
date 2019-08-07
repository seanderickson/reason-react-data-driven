open Belt;
open Common;

let debug_mode = false;

type dataType =
  | String
  | Integer
  | Float
  | Boolean
  | ArrayString;

type displayType =
  | String
  | Integer
  | Float
  | Boolean
  | Date
  | Autosuggest
  | List;

// Try out poly variant: the jsConverter generates the "validatorFromJs" function
[@bs.deriving jsConverter]
type validator = [ | `required | `name | `email];

exception DecodeTypeException(string);

module Vocabulary = {
  type t = {
    // id: int,
    scope: string,
    key: string,
    title: string,
    description: option(string),
  }
  and vocabularies = array(t);

  let decode = json =>
    Json.Decode.{
      key: json |> field("key", string),
      scope: json |> field("scope", string),
      title: json |> field("title", string),
      description: json |> optional(field("description", string)),
    };

  let decodeMany = json => Json.Decode.(json |> array(decode));

  // let create = (~scope:string, ~key: string, ~title: string, ~desc:None ) => {}
};

module Field = {
  type t = {
    // id: int,
    // Resource containing this field
    resource_name: string,
    name: string,
    title: string,
    description: string,
    data_type: dataType,
    // Display type; may imply conversion, e.g. string => date
    display_type: displayType,
    // If field refers to another entity; endpoint for that entity
    ref_endpoint: option(string),
    href_template: option(string),
    validators: option(array(validator)),
    editable: bool,
    vocab_scope: option(string),
    vocabularies: option(Vocabulary.vocabularies),
  }
  and fields = array(t);

  let decode = json =>
    Json.Decode.{
      // id: json |> field("id", int),
      resource_name: json |> field("resource_name", string),
      name: json |> field("name", string),
      title: json |> field("title", string),
      description: json |> field("description", string),
      data_type:
        json
        |> field("data_type", string)
        |> (
          (v) => (
            switch (v) {
            | "string" => String
            | "integer" => Integer
            | "float" => Float
            | "arraystring" => ArrayString
            | "boolean" => Boolean
            | unknownType =>
              raise(
                DecodeTypeException(
                  "Missing type conversion case for field data_type: "
                  ++ unknownType
                  ++ ", JSON: "
                  ++ Js.Json.stringify(json),
                ),
              )
            }: dataType
          )
        ),
      // display_type: json |> field("display_type", string),
      display_type:
        json
        |> field("display_type", string)
        |> (
          (v) => (
            switch (v) {
            | "string" => String
            | "integer" => Integer
            | "float" => Float
            | "list" => List
            | "boolean" => Boolean
            | "date" => Date
            | "autosuggest" => Autosuggest
            | unknownType =>
              raise(
                DecodeTypeException(
                  "Missing type conversion case for field display_type: "
                  ++ unknownType
                  ++ ", JSON: "
                  ++ Js.Json.stringify(json),
                ),
              )
            }: displayType
          )
        ),
      ref_endpoint: json |> optional(field("ref_endpoint", string)),
      href_template: json |> optional(field("href_template", string)),
      validators:
        json
        |> optional(field("validators", array(string)))
        |> Belt.Option.map(_, arrayString =>
             Belt.Array.map(arrayString, v =>
               validatorFromJs(v)->Belt.Option.getExn
             )
           ),
      editable:
        json
        |> optional(field("editable", bool))
        |> Belt.Option.getWithDefault(_, true),
      vocab_scope: json |> optional(field("vocab_scope", string)),
      vocabularies: None,
    };
  let decodeMany = json => Json.Decode.(json |> array(decode));

  let getVocabTitle = (field: t, rawValue: string) => {
    switch (field.vocabularies) {
    | Some(vocabularies) =>
      Belt.Array.getBy(vocabularies, v => v.key == rawValue)
      |> Belt.Option.mapWithDefault(_, rawValue, v => v.title)
    | None => rawValue
    };
  };

  let getDisplayValue = (entity: Js.Json.t, schemaField: t): option(string) => {
    if (debug_mode) {
      Js.log4(
        "decode data field: ",
        schemaField.resource_name,
        schemaField.name,
        entity,
      );
    };
    Json.Decode.(
      switch (schemaField.data_type) {
      | String => entity |> optional(field(schemaField.name, string))
      | Integer =>
        entity
        |> optional(field(schemaField.name, int))
        |> Belt.Option.map(_, string_of_int)
      | Float =>
        entity
        |> optional(field(schemaField.name, Json.Decode.float))
        |> Belt.Option.map(_, Js.Float.toString)
      | ArrayString =>
        entity
        |> optional(field(schemaField.name, array(string)))
        |> Belt.Option.map(_, Js.Array.joinWith(","))
      | Boolean =>
        entity
        |> optional(field(schemaField.name, bool))
        |> Belt.Option.map(_, string_of_bool)
      }
    );
  };
};

module Resource = {
  type t = {
    id: int,
    name: string,
    title: string,
    description: string,
    fields: array(Field.t),
  }
  and resources = array(t);

  let decode = json => {
    Json.Decode.{
      id: json |> field("id", int),
      name: json |> field("name", string),
      title: json |> field("title", string),
      description: json |> field("description", string),
      fields: [||],
    };
  };

  let decodeMany = json => Json.Decode.(json |> array(decode));

  let getField = (resource, fieldName): option(Field.t) => {
    resource.fields |> Array.getBy(_, field => field.name == fieldName);
  };
};

let singleFieldDecode = (json: Js.Json.t, schemaField: Field.t): string => {
  if (debug_mode) {
    Js.log4(
      "decode single field: ",
      schemaField.resource_name,
      schemaField.name,
      json,
    );
  };
  try (
    switch (schemaField.data_type) {
    | String => json |> Json.Decode.string
    | Integer => json |> Json.Decode.int |> string_of_int
    | Float => json |> Json.Decode.float |> Js.Float.toString
    | ArrayString =>
      json |> Json.Decode.(array(string)) |> Js.Array.joinWith(", ")
    | Boolean => json |> Json.Decode.(bool) |> string_of_bool
    }
  ) {
  | Js.Exn.Error(e) =>
    switch (Js.Exn.message(e)) {
    | Some(message) => Js.log2("error", message)
    | None => Js.log("no message")
    };
    Js.log3("Decode error", schemaField.resource_name, schemaField.name);
    Js.log2("json for error", json);
    "";
  | Json.Decode.DecodeError(msg) => msg
  };
};

let nullDecoder = json => json;
let jsonArrayDecoder = Json.Decode.array(nullDecoder);

module Encode = {
  exception UndefinedEncoder(string);

  let encodeField = (field: Field.t, formValue: option(string)): Js.Json.t => {
    let encode = value =>
      switch (field.data_type) {
      | String => value |> Js.Json.string
      | Integer => value |> float_of_string |> Js.Json.number
      | Float => value |> float_of_string |> Js.Json.number
      | Boolean => value |> bool_of_string |> Js.Json.boolean
      | _ =>
        raise(
          UndefinedEncoder({j|Encoder for "$field.name" is not defined|j}),
        )
      };

    switch (formValue) {
    | Some(value) => encode(value)
    | None => Js.Json.null
    };
  };
};