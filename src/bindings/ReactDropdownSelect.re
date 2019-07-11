


[@bs.deriving abstract]
type optionItem = {
  value: string,
  label: string,
};


[@bs.module "react-dropdown-select/dist/react-dropdown-select.js"] [@react.component]
external make:
  (
    ~values: array(string),
    ~options: array(optionItem),
    ~onChange: array(string) => unit,
    ~placeholder: string=?,
    ~multi: bool=?,
    ~searchable: bool=?,
    ~clearable: bool=?,
    unit
  ) =>
  React.element =
  "default" /*     children*/;


