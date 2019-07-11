[@react.component]
let make = () => {
  <input
    name="file"
    type_="file"
    onChange={evt => {
      let input = ReactEvent.Form.target(evt);
      let file = input##files[0];
      Js.log(file);
    }}
  />;
};