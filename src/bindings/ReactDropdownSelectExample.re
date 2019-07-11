[%bs.raw {|require('../css/reactselect.css')|}];
open Common;

type state = {values: array(string) };

[@react.component]
let make = () => {
  let (state, setState) = React.useState(() => {values: [||] });

  let suggestionsAvailable = [|"one", "two", "three", "four", "five"|];
  let suggestionTitles = [|"One", "Two", "Three", "Four", "Five"|];
  let suggestions =
    suggestionsAvailable
    |> Belt.Array.mapWithIndex(_, (index, v) =>
         ReactDropdownSelect.optionItem(
           ~value=v,
           ~label=Belt.Array.getExn(suggestionTitles, index),
         )
       );
  // let onChange = values => {
  //   Js.log2("onChange", values);
  //   let newValue =
  //     switch (Belt.Array.get(values, 0)) {
  //     | Some(optionItem) => ReactDropdownSelect.valueGet(optionItem)
  //     | None => ""
  //     };
  //   setState(_ => {...state, value: newValue});
  // };

  let onChange = values => {
    Js.log2("onChange", values);
    // let newValue =
    //   switch (Belt.Array.get(values, 0)) {
    //   | Some(value) => value
    //   | None => ""
    //   };
    setState(_ => {...state, values});
  };

  <ReactDropdownSelect
    options=suggestions
    values=state.values
    onChange
    multi=true
    searchable=true
    clearable=true
    placeholder="Please enter a value..."
  />;
};