[@bs.module "rc-select/lib/Option.js"] [@react.component]
external make:
  (~value: string, ~title: string, ~children: React.element) => React.element =
  "default" /*     children*/;