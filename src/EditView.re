open Belt;
open Common;
open Store;

type editState('a) =
  | Edit('a)
  | Modified('a)
  | Saving('a)
  | Invalid('a, Js.Dict.t(Js.Dict.t(string)))
  | SaveFail('a, option(string), option(Js.Dict.t(Js.Dict.t(string))))
  | SaveSuccess('a);

type state = editState(Js.Json.t);

let getValue = event => ReactEvent.Form.target(event)##value;

let validateField = (field: field, value: string): option(Js.Dict.t(string)) => {
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
    switch (Belt.Array.size(Js.Dict.keys(errors))) {
    | 0 => None
    | _ => Some(errors)
    };
  | None => None
  };
};

let validateEntity =
    (resource, updatedEntity): option(Js.Dict.t(Js.Dict.t(string))) => {
  let errors: Js.Dict.t(Js.Dict.t(string)) = Js.Dict.empty();

  Belt.Array.forEach(resource.fields, field =>
    switch (Js.Dict.get(updatedEntity, field.name)) {
    | Some(value) =>
      let rawVal = Store.Decode.singleFieldDecode(value, field);
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
let make = (~resource: resource, ~id: string, ~entity: Js.Json.t, ~cancelAction ) => {
  let (state, setState) = React.useState(() => Edit(entity));

  // Store the initial entity for detection of user updates:
  // - decodeObject creates a Js.Dict(Js.Json.t)
  let initialEntity = entity |> Js.Json.decodeObject |> Belt.Option.getExn;

  let getInitialFieldValue = (field: field): option(Js.Json.t) =>
    Js.Dict.get(initialEntity, field.name);

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

  let getCurrentFieldValue = (field: field): option(Js.Json.t) =>
    Js.Dict.get(getUpdatedEntity(), field.name);

  let isFieldModified = (field: field) =>
    getCurrentFieldValue(field) != getInitialFieldValue(field);

  let updateField = (field: field, event) => {
    
    let value = event->getValue;

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
          Store.Encode.encodeField(field, value),
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

  let getFieldError = (field: field): option(string) =>
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

  let getHtmlFieldType = field =>
    switch (field.data_type) {
    | String => "text"
    | Integer => "number"
    | _ => "text"
    };

  let printRow = (field: field) => {
    let value =
      Belt.Option.mapWithDefault(getCurrentFieldValue(field), "", jsonValue =>
        Store.Decode.singleFieldDecode(jsonValue, field)
      );

    <div key={"row-field-" ++ field.name} className="detail_table_row">
      <div className="md:text-right">
        <label
          className={
            "font-bold text-right"
            ++ (isFieldModified(field) ? " bg-orange" : "")
          }
          htmlFor={"inline-" ++ field.name}>
          {str(field.title ++ ": ")}
        </label>
      </div>
      {if (field.editable) {
         // TODO: rework - example select input
         if (field.vocabularies != None) {
           <select
             className="bg-gray-200 appearance-none border-2 border-gray-200 rounded max-w-md w-full py-1 leading-tight"
             id={"inline-" ++ field.name}
             onChange={updateField(field)}
             value>
             {Belt.Option.getExn(field.vocabularies)
              |> Array.map(_, v =>
                   <option key={v.key} value={v.key}> {str(v.title)} </option>
                 )
              |> ReasonReact.array}
           </select>;
         } else
           {
             <input
               className="bg-gray-200 appearance-none border-2 border-gray-200 rounded max-w-md w-full py-1 leading-tight"
               id={"inline-" ++ field.name}
               type_={getHtmlFieldType(field)}
               value
               onChange={updateField(field)}
             />;
           };
           // </div>
       } else {
         <div id={"inline-" ++ field.name} type_={getHtmlFieldType(field)}>
           {str(value)}
         </div>;
       }}

      {
        // FIXME: causes a mouse focus to be lost
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

    ApiClient.patchEntity(resource.name, id, currentEntity)
    |> Js.Promise.then_(result => {
         switch (result) {
         | Result.Ok(entity) =>
           Js.log2("Save success: ", entity);
           setState(_ => SaveSuccess(entity));
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
             Js.log3(
               "SaveFail: err msg decode failure for msg: ",
               x,
               message,
             );
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
         Js.Promise.resolve();
       })
    |> ignore;
  };

  // let cancelAction = () =>
  //   ReasonReactRouter.push("/" ++ resource.name ++ "/" ++ id);

  switch (state) {
  | SaveFail(_, optErrMsg, _) =>
    <div id="entity">
      <button
        style={ReactDOMRe.Style.make(~display="none", ())} onClick={_ => ()}>
        {str("Save")}
      </button>
      <button onClick={_ => cancelAction()}> {str("Cancel")} </button>
      <h3 className="shadow"> {str("Edit entity: " ++ resource.title)} </h3>
      <div className="box-shadow border-red text-red">
        {str("Form Errors!")}
      </div>
      {switch (optErrMsg) {
       | Some(errMsg) => <div className="text-purple"> {str(errMsg)} </div>
       | None => ReasonReact.null
       }}
      {printEntity()}
    </div>
  | Invalid(_, _) =>
    <div id="entity">
      <button
        style={ReactDOMRe.Style.make(~display="none", ())} onClick={_ => ()}>
        {str("Save")}
      </button>
      <button onClick={_ => cancelAction()}> {str("Cancel")} </button>
      <h3 className="shadow"> {str("Edit entity: " ++ resource.title)} </h3>
      <div className="box-shadow border-red text-red">
        {str("Form Errors!")}
      </div>
      {printEntity()}
    </div>
  | Modified(currentEntity) =>
    <div id="entity">
      <button onClick={save(currentEntity)}> {str("Save")} </button>
      <button onClick={_ => cancelAction()}> {str("Cancel")} </button>
      <h3 className="shadow">
        {str("Edit entity: " ++ resource.title ++ "/" ++ id)}
      </h3>
      {printEntity()}
    </div>
  | Edit(_) =>
    <div id="entity">
      // Note: keep the node, with style change, so that DOM update is avoided
      // - otherwise mouse focus is lost when the button appears (on Chrome 74.0)
      <button
        style={ReactDOMRe.Style.make(~display="none", ())} onClick={_ => ()}>
        {str("Save")}
      </button>
      <button onClick={_ => cancelAction()}> {str("Cancel")} </button>
      <h3 className="shadow">
        {str("Edit entity: " ++ resource.title ++ "/" ++ id)}
      </h3>
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
      <button onClick={_ => cancelAction()}> {str("Back")} </button>
      <h3 className="shadow">
        {str("Save success for: " ++ resource.title ++ "/" ++ id)}
      </h3>
      <h3 className="shadow"> {str("JSON posted:")} </h3>
      {newEntity |> printAsJson}
    </div>
  };
};