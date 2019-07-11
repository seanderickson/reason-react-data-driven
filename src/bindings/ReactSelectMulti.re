// Bindings for https://github.com/JedWatson/react-select

[@bs.deriving abstract]
type suggestion = {
  value: string,
  label: string,
};


// Some properties are implemented via the "components" object, see:
// https://react-select.com/props#replacing-components

[@bs.module "react-select/dist/react-select.cjs.js"] [@react.component]
external make:
  (
    ~isMulti: bool,
    ~options: array(suggestion),
    ~value: array(suggestion),
    ~onChange: array(suggestion) => unit,
    ~isClearable: bool=?,
    ~placeholder: string=?,
    ~className: string=?,
    ~classNamePrefix: string=?,
    unit
  ) =>
  React.element =
  "default" /*     children*/;