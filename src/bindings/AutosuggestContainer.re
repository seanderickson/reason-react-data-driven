// Bindings for https://github.com/moroshko/react-autosuggest
// Create a container for Autosuggest to manage the suggestions state

open Common;
open ApiClient;

[%bs.raw {|require('../css/autosuggest.css')|}];

// TODO: implement suggestion fetch state (loading, failed, ready...)

type suggestionState = {
  value: option(string),
  suggestions: array(string),
};

[@react.component]
let make =
    (
      ~id: string,
      ~initialValue: option(string),
      ~getSuggestions: string => apiResult(array(string)),
      ~onChange: option(string) => unit,
      ~containerStyle=?,
      ~containerClasses=?,
    ) => {
  // unit

  let (state: suggestionState, setState) =
    React.useState(() => {value: initialValue, suggestions: [||]});

  let onSuggestionsFetchRequested =
      (valueUpdate: Autosuggest.suggestionRequest): unit => {
    let newValue = Autosuggest.valueGet(valueUpdate);
    let reason = Autosuggest.reasonGet(valueUpdate);
    getSuggestions(newValue)
    |> Js.Promise.then_(result =>
         switch (result) {
         | Belt.Result.Ok(newSugs) =>
           setState(_ => {value: Some(newValue), suggestions: newSugs});
           Js.Promise.resolve();
         | Belt.Result.Error(message) =>
           Js.log2("Todo: error reported on suggestions fetch: ", message);
           Js.Promise.resolve();
         }
       )
    |> ignore;
  };

  let onSuggestionSelected =
      (_evt, data: Autosuggest.onSuggestionSelectedData) => {
    let targetValue = Autosuggest.suggestionValueGet(data);
    let newVal =
      targetValue |> Js.String.trim |> Js.String.length > 0
        ? Some(targetValue) : None;
    setState(_ => {
      onChange(newVal);
      {...state, value: newVal};
    });
  };

  let onSuggestionsClearRequested = () => {
    // Called every time a selection is made
    // Unfortunately, will clear the suggestion made.
    // setState(_ => {...state, suggestions: [||]});
    Js.log2(
      "onSuggestionsClearRequested...",
      state,
    );
  };

  let getSuggestionValue = value => {
    Js.log2("getSuggestionValue", value);
    value;
  };

  let renderSuggestion = value => {
    str(value);
  };

  let onChangeInternal = evt => {
    let v = ReactEvent.Form.target(evt)##value;
    setState(_ => {
      // NOTE: calling onChange here allows the user to define new names
      onChange(v);
      {...state, value: v};
    });
  };

  let inputProps =
    Autosuggest.inputProps(
      ~value=Belt.Option.getWithDefault(state.value, ""),
      ~onChange=onChangeInternal,
      ~placeholder="Enter a value...",
    );
  let printEl = () =>
    <Autosuggest
      id
      suggestions={state.suggestions}
      onSuggestionsFetchRequested
      onSuggestionSelected
      onSuggestionsClearRequested
      getSuggestionValue
      renderSuggestion
      inputProps
    />;
  switch (containerClasses) {
  // | Some(style_) => <div style=style_> {printEl()} </div>
  | Some(classes) => <div className=classes> {printEl()} </div>
  | None => printEl()
  };
};