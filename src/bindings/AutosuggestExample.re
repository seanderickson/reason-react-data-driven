// Bindings for https://github.com/moroshko/react-autosuggest
// Bindings for https://github.com/moroshko/react-autosuggest

open Common;

[%bs.raw {|require('../css/autosuggest.css')|}];

type suggestionState = {
  value: string,
  suggestions: array(string),
};


[@react.component]
let make = () => {
  let (state: suggestionState, setState) =
    React.useState(() => {value: "", suggestions: [||]});

  let suggestionsAvailable = [|"one", "two", "three", "four", "five"|];

  let getSuggestions = (value: string): array(string) => {
    // Can query external server here

    let inputValue = Js.String.trim(value) |> Js.String.toLowerCase;
    let inputLength = inputValue |> Js.String.length;
    inputLength === 0
      ? [||]
      : suggestionsAvailable
        |> Js.Array.filter(v =>
             Js.String.toLowerCase(v)
             |> Js.String.slice(~from=0, ~to_=inputLength) === inputValue
           );
  };

  // Autosuggest will call this function every time you need to update suggestions.
  let onSuggestionsFetchRequested = (valueUpdate: Autosuggest.suggestionRequest) => {
    let newValue = Autosuggest.valueGet(valueUpdate);
    let reason = Autosuggest.reasonGet(valueUpdate);
    let newSugs = getSuggestions(newValue);
    Js.log4("onSuggestionsFetchRequested", newValue, reason, newSugs);
    setState(_ => {value: newValue, suggestions: newSugs});
  };

  let onSuggestionSelected =
      (_evt, data: Autosuggest.onSuggestionSelectedData) => {
    let targetValue = Autosuggest.suggestionValueGet(data);
    Js.log3("onSuggestionSelected:", targetValue, data);
    setState(_ => {...state, value: targetValue});
  };

  let onSuggestionsClearRequested = () =>
    Js.log("onSuggestionsClearRequested");

  let getSuggestionValue = value => {
    Js.log2("getSuggestionValue", value);
    value;
  };

  let renderSuggestion = value => str(value);

  let onChange = evt => {
    ReactEvent.Form.target(evt)##value
    |> (v => setState(_ => {...state, value: v}));
  };

  let inputProps =
    Autosuggest.inputProps(
      ~value=state.value,
      ~onChange,
      ~placeholder="Enter a value...",
    );

  <Autosuggest
    suggestions={state.suggestions}
    onSuggestionsFetchRequested
    onSuggestionSelected
    onSuggestionsClearRequested
    getSuggestionValue
    renderSuggestion
    inputProps
  />;
};