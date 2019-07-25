open Common;

// [%bs.raw {|require('../../node_modules/rc-select/assets/index.css')|}];

/// NOTE: NOT yet working:
// - onSelect, onChange are not being fired
// - needs style override to make the select show??

type state = {value: string};

[@react.component]
let make = () => {
  let (state, setState) = React.useState(() => {value: ""});

  let suggestionsAvailable = [|"one", "two", "three", "four", "five"|];
  let suggestionTitles = [|"One", "Two", "Three", "Four", "Five"|];
  let onChange = (newValue, x) => {
    Js.log3("onChange", newValue, x);
    setState(_ => {...state, value: newValue});
  };
  let onSelect = (newValue, x) => {
    Js.log3("onSelect", newValue, x);
    setState(_ => {...state, value: newValue});
  };
  <div>
    <RcSelect
      style={ReactDOMRe.Style.make(~display="block", ())}
      value={state.value}
      onChange
      onSelect
      // multiple=true
      showSearch=true
      allowClear=true
      placeholder={str("Please enter a value...")}
      >
      {
        Js.log2("render RC-Select options", suggestionsAvailable);
        suggestionsAvailable
        |> Belt.Array.mapWithIndex(_, (index, v) =>
             <RcOption
               value=v
               title={Belt.Array.getExn(suggestionTitles, index)}
               key=v>
               {str(Belt.Array.getExn(suggestionTitles, index))}
             </RcOption>
           )
        |> ReasonReact.array;
      }
    </RcSelect>
  </div>;
};