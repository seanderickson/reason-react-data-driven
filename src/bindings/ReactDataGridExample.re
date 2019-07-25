
// [%bs.raw {|require('../../node_modules/bootstrap/dist/css/bootstrap.css')|}];
open Common;

[@react.component]
let make = () => {
  let columnInput =
    [|[|"col1", "Col1"|], [|"col2", "Col2"|], [|"col3", "Col3"|]|]
    |> Belt.Array.map(_, row =>
         ReactDataGrid.column(~key=row[0], ~name=row[1],())
       );

  Js.log2("Columns: ", columnInput);

  let json = {|  [
      {
        "col1": "value1",
        "col2": 1,
        "col3": 4.1
      }, {
        "col1": "value2",
        "col2": 2,
        "col3": 4.2
      }, {
        "col1": "value3",
        "col2": 3,
        "col3": 4.3
      }, {
        "col1": "value4",
        "col2": 4,
        "col3": 4.4
      }, {
        "col1": "value5",
        "col2": 5,
        "col3": 4.5
      }
      ]
   |};
  Js.log2("raw json: ", json);
  let data = json |> Json.parseOrRaise |> Json.Decode.array(j => j);
  Js.log2("parsed json", data);

  <ReactDataGrid
    columns=columnInput
    rowGetter={i => {
      Js.log2("get data at", i);
      i >= 0 ? Js.Nullable.return(data[i]) : Js.Nullable.null;
    }}
    rowsCount={Belt.Array.size(data)}
    minHeight=500
  />;
};