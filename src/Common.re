
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

type route = 
  | Project
  | Microscope
  | Resource;
  // | Samples
  // | Planned
  // | Actual;


type modalAction = 
  Hide 
  | Show(string, (string)=>unit, (string)=>unit)
  | ModalCancel
  | ModalOk;

type modalState = {
  shown: bool,
  message: string,
  callBackOk: (string)=>unit,
  callBackCancel: (string)=>unit
};

module BMS = Belt.Map.String;

let str = ReasonReact.string;

let reactContextProvider = (~children, ~context, ~value) =>
  React.createElement(
    React.Context.provider(context),
    {"children": children, "value": value},
  );