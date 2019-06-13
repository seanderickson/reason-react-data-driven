// [%bs.raw
//   {|require('./node_modules/react-datepicker/dist/react-datepicker.css')|}
// ];

// // [%bs.raw {|require('moment/locale/en-ca.js')|}];
[@bs.module "react-datepicker/dist/react-datepicker.js"] [@react.component]
external make:
  (
    ~selected: Js.Date.t,
    ~onChange: Js.Date.t => unit,
    ~dateFormat: string=?,
    ~className: string=?,
    unit
  ) =>
  React.element =
  "default" /*     children*/;