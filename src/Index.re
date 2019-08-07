[%bs.raw {|require('./css/simpleForm.css')|}];
[%bs.raw {|require('./css/simpleTable.css')|}];
[%bs.raw {|require('./css/modal.css')|}];
[%bs.raw {|require('./css/reactselect.css')|}];

[%%debugger.chrome];

ReactDOMRe.renderToElementWithId(<App />, "app");