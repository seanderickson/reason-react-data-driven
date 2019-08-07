// Binding for https://github.com/Hacker0x01/react-datepicker/

[@bs.module "react-datepicker/dist/react-datepicker.js"] [@react.component]
external make:
  (
    ~id: string,
    ~selected: option(Js.Date.t),
    ~onChange: Js.Nullable.t(Js.Date.t) => unit,
    ~dateFormat: string=?,
    ~className: string=?,
    ~isClearable: bool=?,
    unit
  ) =>
  React.element =
  "default";