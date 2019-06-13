// type projectState = {
//   projectName: string,
//   pi: string,
//   primaryIrb: string,
//   lspIrb: string,
//   protocolIo: string,
//   scientistFrom: string,
//   pathologyCore: string,
//   coPi: option(string),
//   scientistConducting: string,
//   grant: string,
//   purpose: string,
//   organism: string,
//   sampleSize: option(int),
//   cycles: string,
//   comments: string
// };

// type channel = {
//   ordinal: int,
//   filter: string,
// };

// type channels = list(channel);

// type microscope = {
//   name: string,
//   channels,
//   magnification: string,
// };

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

module BMS = Belt.Map.String;

let str = ReasonReact.string;

let lpad = (input: string, pad: char, targetLen: int): string => {
  input
  ++ (
    Belt.Array.make(String.length(input) - targetLen, pad)
    |> Belt.Array.reduce(_, "", (a, c) => a ++ String.make(1, c))
  );
};

let formatDate = (date: Js.Date.t): string => {
  Js.log2("encodeDate", date);
  let encoded =
    Js.Float.toString(Js.Date.getFullYear(date))
    ++ "-"
    ++ lpad(Js.Float.toString(Js.Date.getMonth(date) +. 1.0), '0', 2)
    ++ "-"
    ++ lpad(Js.Float.toString(Js.Date.getDate(date)), '0', 2);
  Js.log2("encoded Date", encoded);
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

// let decodeJsDate: (raw:Js.Json.t):Js.Date.t =>


let reactContextProvider = (~children, ~context, ~value) =>
  React.createElement(
    React.Context.provider(context),
    {"children": children, "value": value},
  );