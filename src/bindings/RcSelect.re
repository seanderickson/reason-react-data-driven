[@bs.module "rc-select/lib/Select.js"] [@react.component]
external make:
  (
    ~value: string,
    ~onChange: (string, React.element) => unit,
    ~onSelect: (string, React.element) => unit,
    // ~multiple: bool=?,
    ~showSearch: bool=?,
    ~allowClear: bool=?,
    ~placeholder: React.element=?,
    ~style: ReactDOMRe.Style.t=?,
    ~children: React.element,
    unit
  ) =>
  React.element =
  "default" /*     children*/;




// [@bs.deriving abstract]
// type jsProps = {
//     value: string,
//     onChange: (string, React.element) => unit
//     // children: ReasonReact.array
//     // placeholder: React.element
// };


// // [@bs.module "rc-select/lib/Select.js"] external myJSReactClass: ReasonReact.reactClass = "default";
// [@bs.module] external RcSelect: ReasonReact.reactClass = "rc-select/lib/Select.js";

// let make = (children) =>
//   ReasonReact.wrapJsForReason(
//     ~reactClass=myJSReactClass,
//     ~props=jsProps(
//     ~value: string,
//     ~onChange: (string, React.element) => unit
//     // ~placeholder: React.element=?,
//     unit
//     ),
//     children,
//   );