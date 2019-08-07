let handleClick = (href, event) =>
  if (!ReactEvent.Mouse.defaultPrevented(event)) {
    ReactEvent.Mouse.preventDefault(event);
    Js.log2("handleClick", href);
    ReasonReactRouter.push(href);
  };

[@react.component]
let make = (~href, ~selected=false, ~children) =>
  <a href tabIndex=(-1) className={selected ? "selected" : ""} onClick={handleClick(href)}>
    children
  </a>;