open Common;
open Metadata;

[@react.component]
let make =
    (
      ~resource: Resource.t,
      ~entity: Js.Json.t,
      ~entityId=?,
      ~refreshAction,
      ~isNew=false,
      ~saveAction=?,
    ) => {
  let render = (entity: Js.Json.t, state: EntityEditor.state, setState) => {
    module Functor =
      EntityEditor.Make({
        let resource = resource;
        let state = state;
        let setState = setState;
        let entity = entity;
      });

    let printRow = (field: Field.t) => {
      let fieldJsonValue = Functor.getCurrentFieldValue(field);
      if (debug_mode == true) {
        Js.log4("printRow", field.name, "value", fieldJsonValue);
      };
      <div key={"row-field-" ++ field.name} className="detail_table_row">
        <div className="md:text-right">
          <label
            className={
              "font-bold text-right"
              ++ (Functor.isFieldModified(field) ? " bg-orange-300" : "")
            }
            htmlFor={"editor-" ++ field.name}>
            {str(field.title ++ ": ")}
          </label>
        </div>
        {if (field.editable) {
           Functor.printEditor(field);
         } else {
           Functor.printCurrentDisplayValue(field);
         }}
        {// FIXME: causes a mouse focus to be lost: solution is to hide the element until needed
         switch (Functor.getFieldError(field)) {
         | Some(err) => <span className="text-red-500"> {str(err)} </span>
         | None => ReasonReact.null
         }}
      </div>;
    };

    let printErrorMessage = (~msg="Form Errors!", ()) =>
      <div id="form-errors" className="">
        <div className="text-red-500"> {str(msg)} </div>
      </div>;

    let printEntity = () => {
      <form className="detail_table" id="entity_table">
        {resource.fields
         |> Belt.Array.map(_, field => printRow(field))
         |> ReasonReact.array}
      </form>;
    };

    let printAsJson = entity => {
      str(entity |> Js.Json.stringify);
    };

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
        {printErrorMessage(~msg=?optErrMsg, ())}
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
        {printErrorMessage()}
        {printEntity()}
      </div>
    | Modified(currentEntity) =>
      <div id="entity">
        <h3>
          <button
            className="btn btn-gray"
            onClick={_evt =>
              Functor.save(currentEntity, ~isNew, ~entityId, ~saveAction, ())
            }>
            {str("Save")}
          </button>
          <button className="btn btn-gray" onClick={_ => refreshAction()}>
            {str("Cancel")}
          </button>
        </h3>
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
          // <h3 className="shadow">
          //   {str("Edit entity: " ++ resource.title ++ "/" ++ id)}
          // </h3>
          {printEntity()}
        </div>
    | Saving(currentEntity) =>
      <div>
        {
          Js.log2("saving: ", currentEntity);
          str("Saving...");
        }
      </div>
    | SaveSuccess(newEntity) =>
      <div>
        <button className="btn btn-gray" onClick={_ => refreshAction()}>
          {str("Back")}
        </button>
        <h3 className="shadow"> {str("JSON posted:")} </h3>
        {newEntity |> printAsJson}
      </div>
    };
  };

  <EntityEditor entity render />;
};