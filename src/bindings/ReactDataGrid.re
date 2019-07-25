// Bindings for https://github.com/adazzle/react-data-grid
// TODO: incomplete
//
// Outstanding issues:
// Multiline headers: https://stackoverflow.com/questions/44114617/multi-line-headers-in-react-data-grid




[@bs.deriving abstract]
type column = {
  key: string,
  name: string,
  [@bs.optional] width: int,
  [@bs.optional] filterable: bool,
  [@bs.optional] resizable: bool,
  [@bs.optional] sortable: bool,
  [@bs.optional] dragable: bool,
  [@bs.optional] headerRenderer: Js.t({. column:column})=>React.element,
  [@bs.optional] formatter: Js.t({. value:Js.Json.t, isScrolling: bool, row: Js.Json.t})=>React.element
};
[@bs.deriving abstract]
type columns = array(column);

// [@bs.module "react-data-grid/dist/react-data-grid.js"] [@react.component]
// [@bs.module "react-data-grid/index"] [@react.component]
[@bs.module] [@react.component] external make:
  (
    ~columns: columns,
    ~rowGetter: int => Js.Nullable.t(Js.Json.t),
    ~rowsCount: int,
    ~minHeight: int=?,
    ~headerFiltersHeight: int=?,
    ~headerRowHeight: int=?,
    unit
  ) =>
  React.element =
  "react-data-grid/dist/react-data-grid.js" /*     children*/;