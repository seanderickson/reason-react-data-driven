[%bs.raw {|require('./css/simpleForm.css')|}];
[%bs.raw {|require('./css/simpleTable.css')|}];
[%bs.raw {|require('./css/modal.css')|}];
[%bs.raw {|require('./css/reactselect.css')|}];
[%bs.raw
  {|require('../node_modules/react-datepicker/dist/react-datepicker.css')|}
];
[%%debugger.chrome];

ReactDOMRe.renderToElementWithId(<App />, "app");