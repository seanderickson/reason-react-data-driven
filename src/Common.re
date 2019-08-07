let debug_mode = false;
let debug_mode = false;

type modalAction =
  | Hide
  | Show(string, string => unit, string => unit)
  | ModalCancel
  | ModalOk;

type modalState = {
  shown: bool,
  message: string,
  callBackOk: string => unit,
  callBackCancel: string => unit,
};

//module BMS = Belt.Map.String;

let str = ReasonReact.string;
let getEventValue = event => ReactEvent.Form.target(event)##value;
let getValue = getEventValue;

[@bs.val] external document: Dom.document = "document";
[@bs.set] external setTitleDom: (Dom.document, string) => unit = "title";
[@bs.get] external getTitleDom: Dom.document => string = "title";

[@bs.val] external encodeURIComponent: string => string = "encodeURIComponent";
[@bs.val] external decodeURIComponent: string => string = "decodeURIComponent";

let nbsp = [%raw {|'\u00a0'|}];

let arrayPrint = stringArray => {
  let brkStr = (i, errVal) =>
    <div key={string_of_int(i)}>
      {str(errVal)}
      <br key={string_of_int(i)} />
    </div>;
  Belt.Array.mapWithIndex(stringArray, (i, errVal) => brkStr(i, errVal))
  |> ReasonReact.array;
};

let lpad = (input: string, pad: char, targetLen: int): string => {
  (
    Belt.Array.make(targetLen - String.length(input), pad)
    |> Belt.Array.reduce(_, "", (a, c) => a ++ String.make(1, c))
  )
  ++ input;
};

let formatDate = (date: Js.Date.t): string => {
  if (debug_mode) {
    Js.log2("encodeDate", date);
  };
  let encoded =
    Js.Float.toString(Js.Date.getFullYear(date))
    ++ "-"
    ++ lpad(Js.Float.toString(Js.Date.getMonth(date) +. 1.0), '0', 2)
    ++ "-"
    ++ lpad(Js.Float.toString(Js.Date.getDate(date)), '0', 2);
  if (debug_mode) {
    Js.log2("encoded Date", encoded);
  };
  encoded;
};

/**
  Detect invalid Date objects:

  Date.parse may create an "Invalid Date", with no exception.

  consider: https://date-fns.org/, e.g. https://github.com/SllyQ/bs-date-fns
 */
let isJsDateValid: Js.Date.t => bool = [%bs.raw
  {|
  function(date) {
    return (date instanceof Date && !isNaN(date.valueOf())) ? 1 : 0;
  }
|}
];


let altDateParse = (rawString) : option(Js.Date.t) =>{ 
  let dateRegex = [%bs.re {|/(\d+)\/(\d+)\/(\d+)/|}];
  let result =
    Belt.Option.map(
      Js.Re.exec_(dateRegex, Js.String.trim(rawString)), reResult =>
      Js.Re.captures(reResult)
      |> Js.Array.filteri((_, i) => i > 0)
      |> Js.Array.map(Js.Nullable.toOption)
      |> Js.Array.map(x =>
           Belt.Option.mapWithDefault(x, Js.Float._NaN, float_of_string)
         )
      |> (
        vals =>
          Js.Date.makeWithYMD(
            ~year=vals[2],
            ~month=vals[0] -. 1.0,
            ~date=vals[1],
            (),
          )
      )
    );
  if (debug_mode === true) {
    Js.log3("date parse", rawString, result);
  };
  // Note: parsing may produce an "Invalid Date"; detect this as DatePicker will error
  switch (result) {
  | Some(dateVal) =>
    if (isJsDateValid(dateVal)) {
      Some(dateVal);
    } else {
      Js.log2("invalid altDateParse", rawString);
      None;
    }
  | None => None
  };
};


let dateParse = (rawString): option(Js.Date.t) => {
  let dateRegex = [%bs.re {|/(\d+)-(\d+)-(\d+)/|}];
  let result =
    Belt.Option.map(
      Js.Re.exec_(dateRegex, Js.String.trim(rawString)), reResult =>
      Js.Re.captures(reResult)
      |> Js.Array.filteri((_, i) => i > 0)
      |> Js.Array.map(Js.Nullable.toOption)
      |> Js.Array.map(x =>
           Belt.Option.mapWithDefault(x, Js.Float._NaN, float_of_string)
         )
      |> (
        vals =>
          Js.Date.makeWithYMD(
            ~year=vals[0],
            ~month=vals[1] -. 1.0,
            ~date=vals[2],
            (),
          )
      )
    );
  if (debug_mode === true) {
    Js.log3("date parse", rawString, result);
  };
  // Note: parsing may produce an "Invalid Date"; detect this as DatePicker will error
  switch (result) {
  | Some(dateVal) =>
    if (isJsDateValid(dateVal)) {
      Some(dateVal);
    } else {
      Js.log2("invalid date", rawString);
      None;
    }
  | None => None
  };
};

let reactContextProvider = (~children, ~context, ~value) =>
  React.createElement(
    React.Context.provider(context),
    {"children": children, "value": value},
  );