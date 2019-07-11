// Bindings for https://github.com/moroshko/react-autosuggest

[@bs.deriving abstract]
type inputProps = {
  value: string,
  onChange: ReactEvent.Form.t => unit,
  placeholder: string
};

[@bs.deriving abstract]
type suggestionRequest = {
  value: string,
  reason: string
};

[@bs.deriving abstract]
type onSuggestionSelectedData = {
  method: string,
  sectionIndex: option(int),
  suggestion: string,
  suggestionIndex: int,
  suggestionValue: string
};

[@bs.module "react-autosuggest/dist/Autosuggest.js"] [@react.component]
external make:
  (
    ~suggestions: array(string),
    ~onSuggestionsFetchRequested: suggestionRequest => unit,
    ~onSuggestionSelected: (ReactEvent.Form.t,onSuggestionSelectedData) => unit,
    ~onSuggestionsClearRequested: unit => unit,
    ~getSuggestionValue: string => string,
    ~renderSuggestion: string => React.element,
    ~inputProps: inputProps,
    ~id: string=?,
    unit
  ) =>
  React.element =
  "default" /*     children*/;