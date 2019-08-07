open Belt;
open Common;
open Metadata;
open Store;

let processTemplate: (string, Js.Json.t) => string = [%bs.raw
  {|
    function(templateString, templateVars){

      return templateString.replace(/\${(.*?)}/g, (x,g)=> templateVars[g]);

    }
  |}
];

// let processTemplate: (string, Js.Json.t) => string = [%bs.raw
//   {|
//     function(templateString, templateVars){
//       console.log("processTemplate", templateString, templateVars);
//         return new Function("return `"+templateString +"`;").call(templateVars);
//     }
//   |}
// ];

module EntityDetail = {
  let isLinkAbsolute = href =>
    Js.String.toLowerCase(href) |> Js.String.indexOf("http") > (-1);

  let getFieldDisplayValue = (entity, field) => {
    switch (Metadata.Field.getDisplayValue(entity, field)) {
    | Some(fvalue) =>
      let displayValue = Metadata.Field.getVocabTitle(field, fvalue);
      switch (field.ref_endpoint) {
      | Some(endpoint) =>
        <Link href={"/" ++ endpoint ++ "/" ++ fvalue}>
          {str(displayValue)}
        </Link>
      | None =>
        switch (field.href_template) {
        | Some(href_template) =>
          processTemplate(href_template, entity)
          |> (
            href =>
              if (isLinkAbsolute(href)) {
                <a tabIndex=(-1) href> {str(displayValue)} </a>;
              } else {
                <Link href> {str(displayValue)} </Link>;
              }
          )
        | None => str(displayValue)
        }
      };
    | None => str("-")
    };
  };
};

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
  let printRow = (entity, field: Field.t) => {
    <div key={"row-field-" ++ field.name} className="detail_table_row">
      <label
        className="font-bold text-right" htmlFor={"inline-" ++ field.name}>
        {str(field.title ++ ": ")}
      </label>
      <span id={"inline-" ++ field.name} className="text-left">
        {if (Js.Dict.keys(viewFunctionMap) |> Js.Array.includes(field.name)) {
           Js.Dict.unsafeGet(viewFunctionMap, field.name)
           |> (viewFunction => viewFunction(entity) |> str);
         } else {
           EntityDetail.getFieldDisplayValue(entity, field);
         }}
      </span>
    </div>;
  };

  <div>
    <div className="detail_table" id="entity_table">
      {resource.fields
       |> Array.map(_, field => printRow(entity, field))
       |> ReasonReact.array}
    </div>
    children
  </div>;
  // </h3>
  //   {str("Entity detail: " ++ resource.title ++ ": " ++ id)}
  // <h3 className="shadow text-center font-bold">
};