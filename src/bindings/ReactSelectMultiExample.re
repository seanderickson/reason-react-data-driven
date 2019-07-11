[%bs.raw {|require('../css/reactselect.css')|}];
open Common;

type suggestionState = {
  values: array(ReactSelectMulti.suggestion),
  suggestions: array(ReactSelectMulti.suggestion),
};

[@react.component]
let make = () => {
  let (state: suggestionState, setState) =
    React.useState(() =>
      {
        values: [||],
        suggestions: [||],
      }
    );

  let suggestionsAvailable = [|"one", "two", "three", "four", "five"|];
  let suggestionTitles = [|"One", "Two", "Three", "Four", "Five"|];
  let suggestions =
    suggestionsAvailable
    |> Belt.Array.mapWithIndex(_, (index, v) =>
         ReactSelectMulti.suggestion(
           ~value=v,
           ~label=Belt.Array.getExn(suggestionTitles, index),
         )
       );
  Js.log2("suggestions", suggestions);
  let onChange = newValues => {
    Js.log2("react-select multi onChange: ", newValues);
    setState(_ => {...state, values: newValues});
  };

  <ReactSelectMulti
    options=suggestions
    value={state.values}
    onChange
    isClearable=true
    isMulti=true
    placeholder="Please enter a value..."
    classNamePrefix="react_select"
  />;
};