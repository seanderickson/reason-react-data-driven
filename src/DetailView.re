open Belt;
open Common;
open Store;


[@react.component]
let make = (~resource: resource, ~id: string, ~entity: Js.Json.t, ()) => {

  let printRow = (field: field, rvalue) => {
    <div key={"row-field-" ++ field.name} className="detail_table_row">
      <label
        className="font-bold text-right" htmlFor={"inline-" ++ field.name}>
        {str(field.title ++ ": ")}
      </label>
      <span id={"inline-" ++ field.name} className="text-left"> rvalue </span>
    </div>;
  };

  <div>
    <button
      onClick={_ =>
        ReasonReactRouter.push("/" ++ resource.name ++ "/" ++ id ++ "/edit")
      }>
      {str("Edit: " ++ resource.title ++ "/" ++ id)}
    </button>
    <h3 className="shadow">
      {str("Entity detail: " ++ resource.title ++ ": " ++ id)}
    </h3>
    <div className="detail_table" id="entity_table">
      {resource.fields
       |> Array.map(
            _,
            field => {
              let fvalue = Store.Decode.fieldDecoder(entity, field);
              printRow(
                field,
                switch (field.ref_endpoint) {
                | Some(endpoint) =>
                  <Link href={"/" ++ endpoint ++ "/" ++ fvalue} selected=false>
                    {str(fvalue)}
                  </Link>
                | None => str(fvalue)
                },
              );
            },
          )
       |> ReasonReact.array}
    </div>
  </div>;
};