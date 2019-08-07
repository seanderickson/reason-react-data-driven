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
      ~onChange=?,
      ~onInvalid=?,
      ~customEditors=(Js.Dict.empty(): Js.Dict.t(EntityEditor.renderSig)),
      ~showButtons=true,
    ) => {
  let render = (entity: Js.Json.t, state: EntityEditor.state, setState) => {
    module Functor =
      EntityEditor.Make({
        let resource = resource;
        let state = state;
        let setState = setState;
        let entity = entity;
      });

    let printErrorMessage = (~msg="Form Errors!", ()) =>
      <div id="form-errors" className="">
        <div className="text-red-500"> {str(msg)} </div>
      </div>;

    let notifyChange = () => {
      switch (state) {
      | Modified(_) =>
        switch (onChange) {
        | Some(chgFun) =>
          Js.log2("calling chgFun", Functor.getUpdatedEntity());
          chgFun(Functor.getUpdatedEntity() |> Js.Json.object_);
        | None => ()
        }
      | Invalid(_, currErrs) =>
        switch (onInvalid) {
        | Some(invalidFun) =>
          invalidFun(Functor.getUpdatedEntity() |> Js.Json.object_, currErrs)
        | None => ()
        }
      | _ => ()
      };
    };

    let printEntity = () => {
      resource.fields
      |> Belt.Array.mapWithIndex(
           _,
           (i, field) => {
             let classForMod =
               "border border-2 " ++ (Functor.isFieldModified(field) ? "modified-field" : "border-white");
             <td
               key={string_of_int(i) ++ field.name}
               onChange={_evt => notifyChange()}>
               <div className=classForMod>
                 {switch (Js.Dict.get(customEditors, field.name)) {
                  | Some(editFunction) =>
                    editFunction(entity, state, setState)
                  | None =>
                    if (field.editable) {
                      Functor.printEditor(field);
                    } else {
                      Functor.printCurrentDisplayValue(field);
                    }
                  }}
               </div>
             </td>;
           },
         )
      |> ReasonReact.array;
    };

    let printButtons = () => {
      <td key="action-cell" className="cell text-left">
        {switch (state) {
         | Invalid(_, _) =>
           <>
             <button
               className="btn btn-gray"
               style={ReactDOMRe.Style.make(~display="none", ())}
               onClick={_ => ()}>
               {str("Save")}
             </button>
             <button className="btn btn-gray" onClick={_ => refreshAction()}>
               {str("Cancel")}
             </button>
             {printErrorMessage()}
           </>
         | Modified(currentEntity) =>
           <>
             <button
               className="btn btn-gray"
               onClick={_evt =>
                 Functor.save(
                   currentEntity,
                   ~isNew,
                   ~entityId,
                   ~saveAction,
                   (),
                 )
               }>
               {str("Save")}
             </button>
             <button className="btn btn-gray" onClick={_ => refreshAction()}>
               {str("Cancel")}
             </button>
           </>
         | Edit(_) =>
           <>
             <button
               className="btn btn-gray"
               style={ReactDOMRe.Style.make(~display="none", ())}
               onClick={_ => ()}>
               {str("Save")}
             </button>
             <button className="btn btn-gray" onClick={_ => refreshAction()}>
               {str("Cancel")}
             </button>
           </>
         | Saving(_currentEntity) => str("Saving...")
         | SaveSuccess(_newEntity) =>
           <>
             {str("Save success")}
             <button
               className="btn btn-gray"
               style={ReactDOMRe.Style.make(~display="none", ())}
               onClick={_ => ()}>
               {str("Save")}
             </button>
             <button className="btn btn-gray" onClick={_ => refreshAction()}>
               {str("Cancel")}
             </button>
           </>
         | SaveFail(_, optErrMsg, _) =>
           <>
             <button
               style={ReactDOMRe.Style.make(~display="none", ())}
               className="btn btn-gray"
               onClick={_ => ()}>
               {str("Save")}
             </button>
             <button className="btn btn-gray" onClick={_ => refreshAction()}>
               {str("Cancel")}
             </button>
             {printErrorMessage(~msg=?optErrMsg, ())}
           </>
         }}
      </td>;
    };

    <>
      {if (showButtons) {
         printButtons();
       } else {
         ReasonReact.null;
       }}
      {printEntity()}
    </>;
  }; // End render

  <EntityEditor entity render />;
};