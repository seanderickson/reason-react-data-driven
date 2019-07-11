open Belt;
open Common;
open Metadata;
// open Store;
[%bs.raw
  {|require('../node_modules/react-datepicker/dist/react-datepicker.css')|}
];

let debug_mode = false;
// let tailwindInputClasses = "bg-gray-200 appearance-none border-2 border-gray-200 rounded max-w-md w-full py-1 leading-tight";
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

let getValue = event => ReactEvent.Form.target(event)##value;

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
          if (!value->Js.Re.test(emailRegex)) {
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
          if (!value->Js.Re.test(nameRegex)) {
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

[@react.component]
let make =
    (
      ~resource: Resource.t,
      ~id: string,
      ~entity: Js.Json.t,
      ~refreshAction,
      ~isNew=false,
      ~saveAction=?,
    ) => {
  // unit,

  let (state, setState) = React.useState(() => Edit(entity));

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
      |> Array.forEach(_, ((key, v)) => Js.Dict.set(currentCopy, key, v));
      currentCopy;
    };

  let getCurrentFieldValue = (field: Field.t): option(Js.Json.t) =>
    Js.Dict.get(getUpdatedEntity(), field.name)
    |> Belt.Option.flatMap(_, v => v == Js.Json.null ? None : Some(v));

  let isFieldModified = (field: Field.t) => {
    Js.log4(
      "isFieldModified",
      getCurrentFieldValue(field),
      getInitialFieldValue(field),
      getCurrentFieldValue(field) != getInitialFieldValue(field),
    );
    getCurrentFieldValue(field) != getInitialFieldValue(field);
  };

  let updateField = (field: Field.t, value: option(string)) => {
    Js.log3("updateField", field.name, value);

    let updatedEntity = getUpdatedEntity();

    let currentErrors: Js.Dict.t(Js.Dict.t(string)) =
      switch (state) {
      | Invalid(_, errDict) => errDict
      | _ => Js.Dict.empty()
      };

    // Update field values by converting each input to its Js.Json.t equivalent
    try (
      {
        Js.Dict.set(
          updatedEntity,
          field.name,
          Encode.encodeField(field, value),
        );
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

  let getFieldError = (field: Field.t): option(string) =>
    switch (state) {
    | SaveFail(_, optErrMsg, optCurrErrs) =>
      switch (optCurrErrs) {
      | Some(currErrs) =>
        switch (Js.Dict.get(currErrs, field.name)) {
        | Some(fieldErrs) =>
          Some(
            Belt.Array.map(Js.Dict.entries(fieldErrs), ((key, msg)) =>
              {j|$key: $msg|j}
            )
            |> Js.Array.joinWith("\n"),
          )
        | None => None
        }
      | None => None
      }
    | Invalid(_, currErrs) =>
      switch (Js.Dict.get(currErrs, field.name)) {
      | Some(fieldErrs) =>
        Some(
          Belt.Array.map(Js.Dict.entries(fieldErrs), ((key, msg)) =>
            {j|$key: $msg|j}
          )
          |> Js.Array.joinWith("\n"),
        )
      | None => None
      }
    | _ => None
    };

  let getHtmlFieldType = (f: Field.t) =>
    switch (f.data_type) {
    | String => "text"
    | Integer => "number"
    | _ => "text"
    };

  let printRow = (field: Field.t) => {
    let fieldJsonValue = getCurrentFieldValue(field);
    if (debug_mode == true) {
      Js.log4("printRow", field.name, "value", fieldJsonValue);
    };
    let fieldStringValue =
      Belt.Option.mapWithDefault(fieldJsonValue, "", jsonValue =>
        Metadata.singleFieldDecode(jsonValue, field)
      );
    <div key={"row-field-" ++ field.name} className="detail_table_row">
      <div className="md:text-right">
        <label
          className={
            "font-bold text-right"
            ++ (isFieldModified(field) ? " bg-orange-300" : "")
          }
          htmlFor={"inline-" ++ field.name}>
          {str(field.title ++ ": ")}
        </label>
      </div>
      {if (field.editable) {
         let isRequired =
           Belt.Option.mapWithDefault(field.validators, false, validators =>
             Belt.Array.some(validators, v => v == `required)
           );
         // TODO: implement multiselect
         if (field.vocabularies != None) {
           switch (field.display_type) {
           | Autosuggest =>
             let suggestionsAvailable =
               Belt.Option.getExn(field.vocabularies)
               |> Belt.Array.map(_, vocab => vocab.key);
             let fetchSuggestions = (value: string): array(string) => {
               // TODO: Can query external server here
               let inputValue =
                 Js.String.trim(value) |> Js.String.toLowerCase;
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
               id={"inline-" ++ field.name}
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
             let options =
               Belt.Option.getExn(field.vocabularies)
               |> Belt.Array.map(_, vocab =>
                    ReactSelect.suggestion(
                      ~value=vocab.key,
                      ~label=vocab.title,
                    )
                  );

             let suggestionValue =
               Belt.Array.getBy(options, opt =>
                 ReactSelect.valueGet(opt) == fieldStringValue
               );

             <ReactSelect
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
               Belt.Option.map(fieldJsonValue, jsonValue =>
                 Metadata.singleFieldDecode(jsonValue, field)
                 |> Js.Date.fromString
               );

             // Note: Date.parse may produce an "Invalid Date"; detect this as DatePicker will error
             let validFromServer =
               Belt.Option.mapWithDefault(dateValue, true, v =>
                 isJsDateValid(v)
               );

             let dateValue = validFromServer ? dateValue : None;

             if (debug_mode == true) {
               Js.log4(
                 "input date value",
                 dateValue,
                 "validFromServer",
                 validFromServer,
               );
             };
             // TODO: look for better width styling option on DatePicker
             <div style={ReactDOMRe.Style.make(~width="100%", ())}>
               <DatePicker
                 className=tailwindInputClasses
                 //  id={"inline-" ++ field.name}
                 dateFormat="yyyy-MM-dd"
                 selected=dateValue
                 isClearable={!isRequired}
                 onChange={obj =>
                   obj
                   |> Js.Nullable.toOption
                   |> Belt.Option.map(_, v => formatDate(v))
                   |> updateField(field)
                 }
               />
               {if (!validFromServer) {
                  <div className="text-red">
                    {str("Invalid date from server: " ++ fieldStringValue)}
                  </div>;
                } else {
                  ReasonReact.null;
                }}
             </div>;
           //NOTE: Date conversion is performed only on edit; otherwise dates are treated as strings.

           | _ =>
             <input
               className=tailwindInputClasses
               id={"inline-" ++ field.name}
               type_={getHtmlFieldType(field)}
               value=fieldStringValue
               onChange={evt => evt |> getValue |> updateField(field)}
             />
           };
         };
         // </div>
       } else {
         let value =
           Belt.Option.mapWithDefault(fieldJsonValue, "", jsonValue =>
             Metadata.singleFieldDecode(jsonValue, field)
           );
         <div id={"inline-" ++ field.name} type_={getHtmlFieldType(field)}>
           {str(value)}
         </div>;
       }}
      {// FIXME: causes a mouse focus to be lost
       switch (getFieldError(field)) {
       | Some(err) => <span className="text-red"> {str(err)} </span>
       | None => ReasonReact.null
       }}
    </div>;
  };

  let printEntity = () => {
    <form className="detail_table" id="entity_table">
      {resource.fields
       |> Array.map(_, field => printRow(field))
       |> ReasonReact.array}
    </form>;
  };

  let printAsJson = entity => {
    str(entity |> Js.Json.stringify);
  };

  let save = (currentEntity, _evt) => {
    setState(_ => Saving(currentEntity));

    Js.log("Save entity...");

    let processResult = result =>
      switch (result) {
      | Result.Ok(entity) =>
        Js.log2("Save success: ", entity);
        switch (saveAction) {
        | Some(saveActionFunction) =>
          setState(_ => SaveSuccess(entity));
          saveActionFunction(entity);
        | None => setState(_ => SaveSuccess(entity))
        };

      | Result.Error(message) =>
        // Decode a dict(dict(string))
        try (
          {
            let errors: Js.Dict.t(Js.Dict.t(string)) = Js.Dict.empty();

            Js.Json.decodeObject(Json.parseOrRaise(message))
            |> Belt.Option.getExn
            |> Js.Dict.entries
            |> Array.forEach(_, ((key, v)) =>
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

    if (isNew) {
      ApiClient.postEntity(resource.name, currentEntity)
      |> Js.Promise.then_(result => {
           processResult(result);
           Js.Promise.resolve();
         })
      |> ignore;
    } else {
      ApiClient.patchEntity(resource.name, id, currentEntity)
      |> Js.Promise.then_(result => {
           processResult(result);
           Js.Promise.resolve();
         })
      |> ignore;
    };
  };

  let printErrors = () =>
    <div id="form-errors" className="box-shadow border-red text-red">
      {switch (state) {
       | SaveFail(_, optErrMsg, _) =>
         str("Form Errors!");
         switch (optErrMsg) {
         | Some(errMsg) => <div className="text-purple"> {str(errMsg)} </div>
         | None => ReasonReact.null
         };
       | Invalid(_, _) => str("Form Errors!")
       | _ => ReasonReact.null
       }}
    </div>;

  switch (state) {
  | SaveFail(_, optErrMsg, _) =>
    <div id="entity">
      <h3>
        <button
          style={ReactDOMRe.Style.make(~display="none", ())}
          className="btn btn-gray"
          onClick={_ => ()}>
          {str("Save")}
        </button>
        <button className="btn btn-gray" onClick={_ => refreshAction()}>
          {str("Cancel")}
        </button>
      </h3>
      <h3 className="shadow"> {str("Edit entity: " ++ resource.title)} </h3>
      {printErrors()}
      {printEntity()}
    </div>
  | Invalid(_, _) =>
    <div id="entity">
      <h3>
        <button
          className="btn btn-gray"
          style={ReactDOMRe.Style.make(~display="none", ())}
          onClick={_ => ()}>
          {str("Save")}
        </button>
        <button className="btn btn-gray" onClick={_ => refreshAction()}>
          {str("Cancel")}
        </button>
      </h3>
      <h3 className="shadow"> {str("Edit entity: " ++ resource.title)} </h3>
      {printErrors()}
      {printEntity()}
    </div>
  | Modified(currentEntity) =>
    <div id="entity">
      <h3>
        <button className="btn btn-gray" onClick={save(currentEntity)}>
          {str("Save")}
        </button>
        <button className="btn btn-gray" onClick={_ => refreshAction()}>
          {str("Cancel")}
        </button>
      </h3>
      <h3 className="shadow">
        {str("Edit entity: " ++ resource.title ++ "/" ++ id)}
      </h3>
      {printErrors()}
      {printEntity()}
    </div>
  | Edit(_) =>
    <div id="entity">
      // Note: keep the node, with style change, so that DOM update is avoided
      // - otherwise mouse focus is lost when the button appears (on Chrome 74.0)

        <h3>
          <button
            className="btn btn-gray"
            style={ReactDOMRe.Style.make(~display="none", ())}
            onClick={_ => ()}>
            {str("Save")}
          </button>
          <button className="btn btn-gray" onClick={_ => refreshAction()}>
            {str("Cancel")}
          </button>
        </h3>
        <h3 className="shadow">
          {str("Edit entity: " ++ resource.title ++ "/" ++ id)}
        </h3>
        {printErrors()}
        {printEntity()}
      </div>
  | Saving(currentEntity) =>
    <div>
      <h3 className="shadow">
        {str("Saving entity: " ++ resource.title ++ "/" ++ id ++ " ...")}
      </h3>
      {currentEntity |> printAsJson}
    </div>
  | SaveSuccess(newEntity) =>
    <div>
      <button className="btn btn-gray" onClick={_ => refreshAction()}>
        {str("Back")}
      </button>
      <h3 className="shadow">
        {str("Save success for: " ++ resource.title ++ "/" ++ id)}
      </h3>
      <h3 className="shadow"> {str("JSON posted:")} </h3>
      {newEntity |> printAsJson}
    </div>
  };
};