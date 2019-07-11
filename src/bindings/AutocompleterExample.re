/**
  TODO: incomplete example for https://github.com/kraaden/autocomplete

 */


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
    setState(_ => {...state, value: newValue});
  };

  let inputRef = React.useRef(Js.Nullable.null);

  React.useEffect1(
    () => {
      Js.log("Todo...");

      Some(() => Js.log("cleanup Effect"));
    },
    [|state|],
  );

  <div id="selectRef" ref={ReactDOMRe.Ref.domRef(inputRef)} />;
};