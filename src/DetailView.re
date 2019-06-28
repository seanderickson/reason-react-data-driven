open Belt;
open Common;
open Metadata;
open Store;

[@react.component]
let make =
    (
      ~resource: Resource.t,
      ~id: string,
      ~entity: Js.Json.t,
      ~viewFunctionMap=(Js.Dict.empty(): Js.Dict.t(Js.Json.t => string)),
      ~children=ReasonReact.null,
      (),
    ) => {
  let printRow = (field: Field.t, rvalue) => {
    <div key={"row-field-" ++ field.name} className="detail_table_row">
      <label
        className="font-bold text-right" htmlFor={"inline-" ++ field.name}>
        {str(field.title ++ ": ")}
      </label>
      <span id={"inline-" ++ field.name} className="text-left"> rvalue </span>
    </div>;
  };

  let getFieldDisplayValue = (field: Field.t, entity) => {
    switch (Js.Dict.get(viewFunctionMap, field.name)) {
    | Some(viewFunction) => viewFunction(entity)
    | None =>
      Metadata.Field.getVocabTitle(
        field,
        Metadata.fieldDecoder(entity, field),
      )
    };
  };

  <div>
    <h3 className="shadow">
      {str("Entity detail: " ++ resource.title ++ ": " ++ id)}
    </h3>
    <div className="detail_table" id="entity_table">
      {resource.fields
       |> Array.map(
            _,
            field => {
              let rvalue = Metadata.fieldDecoder(entity, field);
              let fvalue = getFieldDisplayValue(field, entity);
              printRow(
                field,
                switch (field.ref_endpoint) {
                | Some(endpoint) =>
                  <Link href={"/" ++ endpoint ++ "/" ++ rvalue} selected=false>
                    {str(fvalue)}
                  </Link>
                | None => str(fvalue)
                },
              );
            },
          )
       |> ReasonReact.array}
    </div>
    children
  </div>;
};