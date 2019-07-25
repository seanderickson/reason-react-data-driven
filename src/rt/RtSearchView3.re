/**
 * Implement a Reagent Tracker search page;
 * - use controlled form components
 * - use reducer to manage searching state
 */
open Common;
open Metadata;

type formAction =
  | NotAsked
  | Search(string)
  | Fail(string)
  | Success(option(array(Js.Json.t)));

type state = {
  formAction,
  searchTerm: option(string),
  errMsg: option(string),
};

type formState = {
  term: string,
  searchType: option(string),
};

type formField =
  | SearchTerm
  | SearchType;

[@react.component]
let make = (~resource: Resource.t, ~initialSearch=None) => {
  Js.log2("Render: ", initialSearch);

  let (formState, setFormState) =
    React.useState(() =>
      {term: Belt.Option.getWithDefault(initialSearch, ""), searchType: None}
    );

  let (state, dispatchState) =
    React.useReducer(
      (state, formAction) => {
        Js.log2("dispatch: ", formAction);
        switch (formAction) {
        | NotAsked => {...state, formAction: NotAsked, searchTerm: None}
        | Search(term) => {
            ...state,
            formAction: Search(term),
            searchTerm: Some(term),
          }
        | Fail(errMsg) => {
            ...state,
            formAction: Fail(errMsg),
            errMsg: Some(errMsg),
          }
        | Success(entities) => {...state, formAction: Success(entities)}
        };
      },
      {formAction: NotAsked, searchTerm: None, errMsg: None},
    );
  // let inputEl = React.useRef(Js.Nullable.null);
  let updateValue = (field, event) => {
    // let value =
    //   switch (getValue(event) |> Js.String.trim) {
    //   | "" => None
    //   | x => Some(x)
    //   };
    let value = getValue(event);
    setFormState(state =>
      switch (field) {
      | SearchTerm => {...state, term: value}
      | SearchType => {...state, searchType: Some(value)}
      }
    );
  };

  let search = term => {
    switch (term) {
    | None => dispatchState(NotAsked)
    | Some(searchString) =>
      dispatchState(Search(searchString));
      ApiClient.searchRtCanonical(searchString)
      |> Js.Promise.then_(result => {
           switch (result) {
           | Belt.Result.Ok(entities) =>
             Js.log2("Search result ok: ", term);
             dispatchState(Success(Some(entities)));
           | Belt.Result.Error(errmsg) => dispatchState(Fail(errmsg))
           };
           Js.Promise.resolve();
         })
      |> ignore;
    };
  };

  // Use the effect hook to detect render with a new initial search term.
  React.useEffect1(
    () => {
      setFormState(state =>
        {...state, term: Belt.Option.getWithDefault(initialSearch, "")}
      );
      Js.log2("Initialsearch: ", initialSearch);
      if (state.searchTerm != initialSearch && initialSearch != None) {
        search(initialSearch);
      };
      Some(() => Js.log("cleanup Effect"));
    },
    [|initialSearch|],
  );

  let printCheckboxes = () => {
    <div id="checkboxContainer" className="">
      <div>
        <input
          type_="radio"
          id="sm"
          name="type"
          value="small_molecule"
          onChange={updateValue(SearchType)}
          checked={formState.searchType == Some("small_molecule")}
        />
        <label htmlFor="sm"> {str("Small Molecule")} </label>
      </div>
      <div>
        <input
          type_="radio"
          id="ab"
          name="type"
          value="antibody"
          onChange={updateValue(SearchType)}
          checked={formState.searchType == Some("antibody")}
        />
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
          let cleanedTerm = Js.String.trim(formState.term);

          if (state.searchTerm != Some(cleanedTerm)) {
            // NOTE: Set route - only for user action & only if different.
            Js.log2("pushing new term: ", cleanedTerm);
            ReasonReactRouter.push("search?q=" ++ cleanedTerm);
            setTitleDom(
              document,
              "Cycif proto: " ++ "search?q=" ++ cleanedTerm,
            );
          } else {
            Js.log2("Search term the same: ", cleanedTerm);
          };

          search(
            Js.String.length(cleanedTerm) > 0 ? Some(cleanedTerm) : None,
          );
        }}>
        <div className="detail_table_row">
          <div className="p-2 text-right min-w-full">
            <label htmlFor="term" className="font-bold text-right">
              {str("Search: ")}
            </label>
          </div>
          <div className="min-w-full">
            <input
              // ref={ReactDOMRe.Ref.domRef(inputEl)}
              value={formState.term}
              onChange={updateValue(SearchTerm)}
              tabIndex=(-1)
              type_="text"
              placeholder="search term"
              id="term"
              className="appearance-none border-2 rounded  py-1 leading-tight"
              // defaultValue={Belt.Option.getWithDefault(searchTerm, "")}
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

  switch (state.formAction) {
  | NotAsked => printSearchForm(~searchTerm=None, ())
  | Fail(errmsg) =>
    <div>
      {str("Search faiure: " ++ errmsg)}
      {printSearchForm(~searchTerm=state.searchTerm, ())}
    </div>
  | Search(term) => str("Searching for: \"" ++ term ++ {j|" \u2026 |j})
  | Success(resultArray) =>
    <div>
      {printSearchForm(~searchTerm=state.searchTerm, ())}
      <EntityListingTable
        key={resource.name}
        resource
        initialState={Store.LoadSuccess(resultArray)}
      />
    </div>
  };
};