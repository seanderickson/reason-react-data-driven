open Common;
open Metadata;

let tailwindInputClasses = "appearance-none border-2 rounded max-w-md w-full py-1 leading-tight";
let tailwindInputWrapperClasses = "appearance-none p-0 rounded max-w-md w-full leading-tight";

type editState('a) =
  | Edit('a)
  | Modified('a)
  | Saving('a)
  | Invalid('a, Js.Dict.t(Js.Dict.t(string)))
  | SaveFail('a, option(string), option(Js.Dict.t(Js.Dict.t(string))))
  | SaveSuccess('a);

type state = editState(Js.Json.t);

let flattenErrors = (currErrs: Js.Dict.t(Js.Dict.t(Js.Dict.key))) => {
  let printFieldErrors = fieldErrs =>
    Belt.Array.map(Js.Dict.entries(fieldErrs), ((key, msg)) =>
      key == msg ? key : {j|$key: $msg|j}
    )
    |> Js.Array.joinWith("\n");
  Belt.Array.map(
    Js.Dict.entries(currErrs),
    ((fieldKey, fieldErrs)) => {
      let msg = printFieldErrors(fieldErrs);
      {j|$fieldKey: $msg|j};
    },
  );
  // |> Js.Array.joinWith("\n");
};

let validateField =
    (field: Field.t, value: string): option(Js.Dict.t(string)) => {
  switch (field.validators) {
  | Some(validators) =>
    let errors: Js.Dict.t(string) = Js.Dict.empty();
    Belt.Array.forEach(validators, validator =>
      switch (validator) {
      | `required =>
        if (Js.String.length(Js.String.trim(value)) == 0) {
          Js.Dict.set(errors, "required", "required");
        }
      | `email =>
        if (Js.String.length(Js.String.trim(value)) == 0) {
          let emailRegex = [%bs.re {|/.*@.*\..+/|}];
          if (!Js.Re.test_(emailRegex, value)) {
            Js.Dict.set(
              errors,
              "email",
              "Email does not fit format: [name]@[host].[domain]",
            );
          };
        }
      | `name =>
        if (Js.String.length(Js.String.trim(value)) == 0) {
          Js.Dict.set(errors, "name", "May not be empty");
        } else {
          let nameRegex = [%bs.re {|/^[\w\s]+$/|}];
          if (!Js.Re.test_(nameRegex, value)) {
            Js.Dict.set(
              errors,
              "name",
              "Name must only include alphanumeric characters and spaces",
            );
          };
        }
      }
    );
    if (field.display_type == Date) {
      if (!(Js.Date.fromString(value) |> isJsDateValid)) {
        Js.Dict.set(errors, "date", "Invalid date");
      };
    };

    switch (Belt.Array.size(Js.Dict.keys(errors))) {
    | 0 => None
    | _ => Some(errors)
    };
  | None => None
  };
};

let validateEntity =
    (resource: Resource.t, updatedEntity)
    : option(Js.Dict.t(Js.Dict.t(string))) => {
  let errors: Js.Dict.t(Js.Dict.t(string)) = Js.Dict.empty();

  Belt.Array.forEach(resource.fields, field =>
    switch (Js.Dict.get(updatedEntity, field.name)) {
    | Some(value) =>
      let rawVal = Metadata.singleFieldDecode(value, field);
      switch (validateField(field, rawVal)) {
      | Some(fieldErrorDict) =>
        Js.Dict.set(errors, field.name, fieldErrorDict)
      | None => ()
      };
    | None => ()
    }
  );

  switch (Belt.Array.size(Js.Dict.keys(errors))) {
  | 0 => None
  | _ => Some(errors)
  };
};

module type State = {
  let state: state;
  let setState: (state => state) => unit;
  let resource: Resource.t;
  let entity: Js.Json.t;
};

module type IF = {
  include State;

  let initialEntity: Js.Dict.t(Js.Json.t);
  let getInitialFieldValue: Field.t => option(Js.Json.t);
  let getUpdatedEntity: unit => Js.Dict.t(Js.Json.t);
  let getCurrentFieldValue: Field.t => option(Js.Json.t);
  let isFieldModified: Field.t => bool;
  let updateField: (Field.t, option(string)) => unit;
  let updateFieldRaw: (Field.t, Js.Json.t) => unit;
  let getFieldError: Field.t => option(string);
  let printEditor: Field.t => ReasonReact.reactElement;
  let printCurrentDisplayValue: Field.t => ReasonReact.reactElement;

  let save:
    (
      Js.Json.t,
      ~isNew: bool,
      ~entityId: option(string),
      ~saveAction: option(Js.Json.t => unit),
      unit
    ) =>
    unit;
};

module Make = (StateContainer: State) : IF => {
  let state = StateContainer.state;
  let setState = StateContainer.setState;
  let resource = StateContainer.resource;
  // let id = StateContainer.id;
  let entity = StateContainer.entity;
  // let refreshAction = StateContainer.refreshAction;
  // let isNew = StateContainer.isNew;
  // let saveAction = StateContainer.saveAction;

  // Store the initial entity for detection of user updates:
  // - Note: decodeObject creates a Js.Dict(Js.Json.t)
  let initialEntity = entity |> Js.Json.decodeObject |> Belt.Option.getExn;

  let getInitialFieldValue = (field: Field.t): option(Js.Json.t) =>
    Js.Dict.get(initialEntity, field.name)
    |> Belt.Option.flatMap(_, v => v == Js.Json.null ? None : Some(v));

  // Track the current entity state:
  // - Use a Js.Dict with the current field values
  let getUpdatedEntity = (): Js.Dict.t(Js.Json.t) =>
    switch (state) {
    | SaveFail(x, _, _)
    | Invalid(x, _)
    | Modified(x) => x |> Js.Json.decodeObject |> Belt.Option.getExn
    | _ =>
      // If not yet modified, then create a new entity to track the updated entity state.
      // NOTE: apparently "initialEntity" is still tied to "entity", so it must be copied over here...
      let currentCopy = Js.Dict.empty();
      initialEntity
      |> Js.Dict.entries
      |> Belt.Array.forEach(_, ((key, v)) =>
           Js.Dict.set(currentCopy, key, v)
         );
      currentCopy;
    };

  let getCurrentFieldValue = (field: Field.t): option(Js.Json.t) =>
    Js.Dict.get(getUpdatedEntity(), field.name)
    |> Belt.Option.flatMap(_, v => v == Js.Json.null ? None : Some(v));

  let isFieldModified = (field: Field.t) => {
    if (debug_mode) {
      Js.log4(
        "isFieldModified",
        field.name,
        getCurrentFieldValue(field),
        getInitialFieldValue(field),
      );
    };
    getCurrentFieldValue(field) != getInitialFieldValue(field);
  };

  let updateFieldRaw = (field: Field.t, value: Js.Json.t) => {
    Js.log3("updateField", field.name, value);
    // TODO: use an immutable (Belt.Map)
    let updatedEntity = getUpdatedEntity();

    let currentErrors: Js.Dict.t(Js.Dict.t(string)) =
      switch (state) {
      | Invalid(_, errDict) => errDict
      | _ => Js.Dict.empty()
      };
    // Update field values by converting each input to its Js.Json.t equivalent
    try (
      {
        Js.Dict.set(updatedEntity, field.name, value);
        switch (validateEntity(resource, updatedEntity)) {
        | Some(reportedErrors) =>
          setState(_ =>
            Invalid(updatedEntity |> Js.Json.object_, reportedErrors)
          )
        | None => setState(_ => Modified(updatedEntity |> Js.Json.object_))
        };
      }
    ) {
    | Js.Exn.Error(e) =>
      let fieldErrorDict: Js.Dict.t(string) = Js.Dict.empty();
      switch (Js.Exn.message(e)) {
      | Some(message) =>
        Js.Dict.set(fieldErrorDict, "Js.Exn", message);
        Js.Dict.set(currentErrors, field.name, fieldErrorDict);
        setState(_ =>
          Invalid(updatedEntity |> Js.Json.object_, currentErrors)
        );
      | None => Js.log2("An unknown error occurred on field: ", field.name)
      };
    | x =>
      let fieldErrorDict: Js.Dict.t(string) = Js.Dict.empty();
      Js.Dict.set(fieldErrorDict, "General Exception", {j|$x|j});
      Js.Dict.set(currentErrors, field.name, fieldErrorDict);
      setState(_ => Invalid(updatedEntity |> Js.Json.object_, currentErrors));
    };
  };

  let updateField = (field: Field.t, value: option(string)) =>
    updateFieldRaw(field, Encode.encodeField(field, value));

  let getFieldError = (field: Field.t): option(string) => {
    let printErrors = fieldErrs =>
      Some(
        Belt.Array.map(Js.Dict.entries(fieldErrs), ((key, msg)) =>
          key == msg ? key : {j|$key: $msg|j}
        )
        |> Js.Array.joinWith("\n"),
      );

    switch (state) {
    | SaveFail(_, _optErrMsg, optCurrErrs) =>
      switch (optCurrErrs) {
      | Some(currErrs) =>
        switch (Js.Dict.get(currErrs, field.name)) {
        | Some(fieldErrs) => printErrors(fieldErrs)
        | None => None
        }
      | None => None
      }
    | Invalid(_, currErrs) =>
      switch (Js.Dict.get(currErrs, field.name)) {
      | Some(fieldErrs) => printErrors(fieldErrs)
      | None => None
      }
    | _ => None
    };
  };

  /** TODO: rework schema for editField */
  let printEditor = (field: Field.t) => {
    let fieldJsonValue = getCurrentFieldValue(field);
    if (debug_mode == true) {
      Js.log4("printRow", field.name, "value", fieldJsonValue);
    };
    let fieldStringValue =
      Belt.Option.mapWithDefault(fieldJsonValue, "", jsonValue =>
        Metadata.singleFieldDecode(jsonValue, field)
      );
    let isRequired =
      Belt.Option.mapWithDefault(field.validators, false, validators =>
        Belt.Array.some(validators, v => v == `required)
      );
    if (field.vocabularies != None) {
      switch (field.display_type) {
      | Autosuggest =>
        let suggestionsAvailable =
          Belt.Option.getExn(field.vocabularies)
          |> Belt.Array.map(_, vocab => vocab.key);
        Js.log2("suggestionsAvailable", suggestionsAvailable);
        let fetchSuggestions = (value: string): array(string) => {
          // TODO: Can query external server here
          let inputValue = Js.String.trim(value) |> Js.String.toLowerCase;
          let inputLength = inputValue |> Js.String.length;
          let sugs =
            inputLength === 0
              ? [||]
              : suggestionsAvailable
                |> Js.Array.filter(v =>
                     Js.String.toLowerCase(v)
                     |> Js.String.splitByRe([%re "/\\s+/"])
                     |> Belt.Array.some(_, v =>
                          Belt.Option.getExn(v)
                          |> Js.String.slice(~from=0, ~to_=inputLength)
                          === inputValue
                        )
                   );
          Js.log3("fetchSuggestions", value, sugs);
          sugs;
        };

        <AutosuggestContainer
          id={"editor-" ++ field.name}
          initialValue={Some(fieldStringValue)}
          onChange={v => updateField(field, v)}
          getSuggestions={v =>
            Js.Promise.resolve(Belt.Result.Ok(fetchSuggestions(v)))
          }
          containerClasses=tailwindInputWrapperClasses
          //  containerStyle={ReactDOMRe.Style.make(
          //    ~width="100%",
          //    ~maxWidth="40rem",
          //    (),
          //  )}
        />;
      | _ =>
        // TODO: implement multiselect
        let options =
          Belt.Option.getExn(field.vocabularies)
          |> Belt.Array.map(_, vocab =>
               ReactSelect.suggestion(~value=vocab.key, ~label=vocab.title)
             );

        let suggestionValue =
          Belt.Array.getBy(options, opt =>
            ReactSelect.valueGet(opt) == fieldStringValue
          );

        <ReactSelect
          id={"editor-" ++ field.name}
          className=tailwindInputWrapperClasses
          classNamePrefix="react_select"
          options
          onChange={suggestion =>
            Js.Nullable.toOption(suggestion)
            |> Belt.Option.map(_, v => ReactSelect.valueGet(v))
            |> updateField(field)
          }
          value=suggestionValue
          isClearable={!isRequired}
          placeholder="Select a value..."
        />;
      };
    } else {
      switch (field.display_type) {
      | Date =>
        let dateValue =
          switch (
            Belt.Option.flatMap(fieldJsonValue, jsonValue =>
              dateParse(Metadata.singleFieldDecode(jsonValue, field))
            )
          ) {
          | Some(v) => Some(v)
          | None =>
            Belt.Option.flatMap(fieldJsonValue, jsonValue =>
              altDateParse(Metadata.singleFieldDecode(jsonValue, field))
            )
          };

        Js.log3("dateValue", fieldJsonValue, dateValue);

        // TODO: look for better width styling option on DatePicker
        <div style={ReactDOMRe.Style.make(~width="100%", ())}>
          <DatePicker
            className=tailwindInputClasses
            id={"editor-" ++ field.name}
            dateFormat="yyyy-MM-dd"
            selected=dateValue
            isClearable={!isRequired}
            onChange={rawVal =>
              rawVal
              |> Js.Nullable.toOption
              |> Belt.Option.map(_, v => formatDate(v))
              |> updateField(field)
            }
          />
          {if (!Belt.Option.isSome(fieldJsonValue)
               && Belt.Option.isNone(dateValue)) {
             Js.log3("Invalid date: ", fieldJsonValue, dateValue);
             <div className="text-red">
               {str("Invalid date from server: " ++ fieldStringValue)}
             </div>;
           } else {
             ReasonReact.null;
           }}
        </div>;
      //NOTE: Date conversion is performed only on edit; otherwise dates are treated as strings.

      | _ =>
        let getHtmlFieldType = (f: Field.t) =>
          switch (f.data_type) {
          | String => "text"
          | Integer => "number"
          | _ => "text"
          };

        <input
          className=tailwindInputClasses
          id={"editor-" ++ field.name}
          type_={getHtmlFieldType(field)}
          value=fieldStringValue
          onChange={evt => evt |> getEventValue |> updateField(field)}
        />;
      };
    };
  };

  let printCurrentDisplayValue = field =>
    getUpdatedEntity()
    |> Js.Json.object_
    |> DetailView.EntityDetail.getFieldDisplayValue(_, field);

  let save = (currentEntity, ~isNew, ~entityId, ~saveAction, ()) => {
    setState(_ => Saving(currentEntity));

    Js.log("Save entity...");
    // let saveAction = Some(_ => ());
    let processResult = result =>
      switch (result) {
      | Belt.Result.Ok(entity) =>
        Js.log2("Save success: ", entity);
        switch (saveAction) {
        | Some(saveActionFunction) =>
          setState(_ => SaveSuccess(entity));
          saveActionFunction(entity);
        | None => setState(_ => SaveSuccess(entity))
        };

      | Belt.Result.Error(message) =>
        // Decode a dict(dict(string))
        try (
          {
            let errors: Js.Dict.t(Js.Dict.t(string)) = Js.Dict.empty();

            Js.Json.decodeObject(Json.parseOrRaise(message))
            |> Belt.Option.getExn
            |> Js.Dict.entries
            |> Belt.Array.forEach(_, ((key, v)) =>
                 Js.Dict.set(errors, key, v |> Json.Decode.(dict(string)))
               );

            setState(_ => SaveFail(currentEntity, None, Some(errors)));
          }
        ) {
        | x =>
          Js.log3("SaveFail: err msg decode failure for msg: ", x, message);
          setState(_ =>
            SaveFail(
              currentEntity,
              Some({j|Failure on error decode: $message|j}),
              None,
            )
          );
        // // Decode the message? (or should be already decoded)
        // let errors: Js.Dict.t(Js.Dict.t(string)) = Js.Dict.empty();
        // Js.log2("Err response: ", message);
        // setState(_=> SaveFail(currentEntity, None, Some(errors)));
        }
      };
    entityId |> Belt.Option.isNone || isNew
      ? ApiClient.postEntity(resource.name, currentEntity)
        |> Js.Promise.then_(result => {
             processResult(result);
             Js.Promise.resolve();
           })
        |> ignore
      : ApiClient.patchEntity(
          resource.name,
          Belt.Option.getExn(entityId),
          currentEntity,
        )
        |> Js.Promise.then_(result => {
             processResult(result);
             Js.Promise.resolve();
           })
        |> ignore;
  };
};

type renderSig =
  (Js.Json.t, state, (state => state) => unit) => ReasonReact.reactElement;

[@react.component]
let make = (~render: renderSig, ~entity: Js.Json.t, ~initialState=?) => {
  let (state, setState) =
    React.useState(() =>
      Belt.Option.getWithDefault(initialState, Edit(entity))
    );

  render(entity, state, setState);
};