// Bindings for https://github.com/kraaden/autocomplete
// TODO: incomplete
[@bs.deriving abstract]
type suggestion = {
  value: string,
  label: string,
};

[@bs.module "autocompleter/autocomplete.js"] [@react.component]
external make:
  (
    ~input: React.Ref.t(Js.Nullable.t('a)),
    ~fetch: (string, array(suggestion) => unit) => unit,
    ~onSelect: suggestion => unit,
    unit
  ) =>
  React.element =
  "default" /*     children*/;