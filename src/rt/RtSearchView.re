/**
 * Implement a Reagent Tracker search page;
 * - use uncontrolled form components
 * - use setState to manage searching state
 */

open Common;
open Metadata;

type searchState =
  | NotAsked
  | Searching(string)
  | SearchFailure(string, string)
  | LoadSuccess(string, option(array(Js.Json.t)));

[@react.component]
let make = (~resource: Resource.t, ~initialSearch=None) => {
  Js.log2("Render: ", initialSearch);
  let (state, setState) = React.useState(() => NotAsked);
  let getTerm = (testState: searchState): string =>
    switch (testState) {
    | NotAsked => ""
    | Searching(term)
    | SearchFailure(term, _)
    | LoadSuccess(term, _) => term
    };

  let inputEl = React.useRef(Js.Nullable.null);

  let search = term => {
    let searchString = Js.String.trim(term);
    if (Js.String.length(searchString) == 0) {
      setState(_ => NotAsked);
    } else {
      setState(_ => Searching(term));
      ApiClient.searchRtCanonical(term)
      |> Js.Promise.then_(result => {
           switch (result) {
           | Belt.Result.Ok(entities) =>
             Js.log2("Search result ok: ", term);
             setState(_ => LoadSuccess(term, Some(entities)));
           | Belt.Result.Error(errmsg) =>
             setState(_ => SearchFailure(errmsg, term))
           };
           Js.Promise.resolve();
         })
      |> ignore;
    };
  };

  // Use the effect hook to detect render with a new initial search term.
  React.useEffect1(
    () => {
      Js.log2("Initialsearch: ", initialSearch);
      if (Some(getTerm(state)) != initialSearch && initialSearch != None) {
        search(Belt.Option.getExn(initialSearch));
      };
      Some(() => Js.log("cleanup Effect"));
    },
    [|initialSearch|],
  );

  let printCheckboxes = () => {
    <div id="checkboxContainer" className="">
      <div>
        <input type_="radio" id="sm" name="type" value="small_molecule" />
        <label htmlFor="sm"> {str("Small Molecule")} </label>
      </div>
      <div>
        <input type_="radio" id="ab" name="type" value="antibody" />
        <label htmlFor="ab"> {str("Antibody")} </label>
      </div>
    </div>;
  };

  let printSearchForm = (~searchTerm, ()) => {
    <div>
      {str(
         "Current search: \""
         ++ Belt.Option.getWithDefault(searchTerm, "")
         ++ "\"",
       )}
      <form
        className="detail_table"
        onSubmit={event => {
          ReactEvent.Synthetic.preventDefault(event);
          let domForm = ReactEvent.Form.target(event);
          Js.log3(
            "submit: domForm##elements: ",
            domForm##elements##sm##checked,
            domForm##elements##ab##checked,
          );
          switch (inputEl->React.Ref.current->Js.Nullable.toOption) {
          | Some(_ref) =>
            let term = ReactDOMRe.domElementToObj(_ref)##value;
            if (getTerm(state) != term) {
              Js.log2("pushing new term: ", term);
              ReasonReactRouter.push("search?q=" ++ term);
              setTitleDom(document, "Cycif proto: " ++ "search?q=" ++ term);
            } else {
              Js.log2("Search term the same: ", term);
            };

            search(term);
          | None => ()
          };
        }}>
        <div className="detail_table_row">
          <div className="p-2 text-right min-w-full">
            <label htmlFor="term" className="font-bold text-right">
              {str("Search: ")}
            </label>
          </div>
          <div className="min-w-full">
            <input
              ref={ReactDOMRe.Ref.domRef(inputEl)}
              tabIndex=(-1)
              type_="text"
              placeholder="search term"
              id="term"
              className="appearance-none border-2 rounded  py-1 leading-tight"
              defaultValue={Belt.Option.getWithDefault(searchTerm, "")}
            />
            {printCheckboxes()}
            <button type_="submit" className="btn btn-gray">
              {str("submit")}
            </button>
          </div>
        </div>
      </form>
    </div>;
  };

  switch (state) {
  | NotAsked => printSearchForm(~searchTerm=None, ())
  | SearchFailure(errmsg, term) =>
    <div>
      {str("Search faiure: " ++ errmsg)}
      {printSearchForm(~searchTerm=Some(term), ())}
    </div>
  | Searching(term) => str("Searching for: \"" ++ term ++ {j|" \u2026 |j})
  | LoadSuccess(term, resultArray) =>
    Js.log3("loadSuccess", term, resultArray);
    <div>
      {printSearchForm(~searchTerm=Some(term), ())}
      <EntityListingTable
        key={resource.name}
        resource
        initialState={Store.LoadSuccess(resultArray)}
      />
    </div>;
  };
};