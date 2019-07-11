[%bs.raw {|require('../css/reactselect.css')|}];
open Common;

type suggestionState = {
  value: option(ReactSelect.suggestion),
  suggestions: array(ReactSelect.suggestion),
};

[@react.component]
let make = () => {
  let (state: suggestionState, setState) =
    React.useState(() =>
      {
        // value: ReactSelect.suggestion(~value="", ~label=""),
        // value: ReactSelect.suggestion(~value=Js.Nullable.null, ~label=""),
        value: None,
        suggestions: [||],
      }
    );

  let suggestionsAvailable = [|"one", "two", "three", "four", "five"|];
  let suggestionTitles = [|"One", "Two", "Three", "Four", "Five"|];
  let suggestions =
    suggestionsAvailable
    |> Belt.Array.mapWithIndex(_, (index, v) =>
         ReactSelect.suggestion(
           ~value=v,
           ~label=Belt.Array.getExn(suggestionTitles, index),
         )
       );
  Js.log2("suggestions", suggestions);
  let onChange = newValue => {
    Js.log2("onChange: ", newValue);

    let stateValue = Js.Nullable.toOption(newValue);

    switch (stateValue) {
    | Some(v) => Js.log2("some", v)
    | None => Js.log("none...")
    };
    setState(_ => {...state, value: stateValue});
  };

  <ReactSelect
    options=suggestions
    value={state.value}
    onChange
    isClearable=true
    // isMulti=true
    placeholder="Please enter a value..."
    classNamePrefix="react_select"
  />;
};