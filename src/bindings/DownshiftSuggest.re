// Bindings for https://github.com/downshift-js/downshift
// NOTE: not functional;
// - Downshift not well suite to binding due to complexity of config types

[@bs.module "downshift/dist/downshift.umd.js"] [@react.component]
external make:
  (
    ~suggestions: array(string),
  ) =>
  React.element =
  "default" /*     children*/;
  