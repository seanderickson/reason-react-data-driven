/**
  A React context (store) for metadata about the API data model:
  - Resource:  API resources,
  - Field: individual fields to be displayed
  - Vocabulary: values and titles for vocabularies

  React context design is based on these examples:
    https://kentcdodds.com/blog/application-state-management-with-react
  - and this adaptation for reason-react:
    https://github.com/mixvar/reason-react-context-playground
 */
open Belt;
open Metadata;

let debug_mode = false;

// Resource Context

type webLoadingData('a) =
  | NotAsked
  | Loading
  | LoadFailure(string)
  | LoadSuccess('a);

module ResourceContext = {
  type resourceState = webLoadingData(option(Resource.resources));

  type t = {
    resourceState,
    fetchResources: unit => unit,
    getResources: unit => option(Resource.resources),
    getResource: string => option(Resource.t),
  };

  let reactContext = React.createContext(None);

  module Provider = {
    [@react.component]
    let make = (~children) => {
      let (resourceState, setResourceState) = React.useState(() => NotAsked);

      let fetchResources = () => {
        setResourceState(_ => Loading);
        ApiClient.buildResources()
        |> Js.Promise.then_(result => {
             switch (result) {
             | Result.Ok(resources) =>
               Js.log("Built resources...");
               setResourceState(_ => LoadSuccess(Some(resources)));
             | Result.Error(message) =>
               setResourceState(_ => LoadFailure(message))
             };
             Js.Promise.resolve();
           })
        |> ignore;
      };

      let getResources = () =>
        switch (resourceState) {
        | LoadSuccess(resources) => resources
        | _ => None
        };

      let getResource = (resourceName): option(Resource.t) => {
        switch (resourceState) {
        | LoadSuccess(resources) =>
          resources->Belt.Option.flatMap(rlist =>
            rlist->Belt.Array.getBy(resource => resource.name == resourceName)
          )
        | _ => None
        };
      };

      React.useEffect0(() => {
        Js.log("Initial fetch...");
        fetchResources();
        Some(() => Js.log("cleanup Effect"));
      });

      let ctx: option(t) =
        Some({resourceState, fetchResources, getResources, getResource})
        |> (it => React.useMemo1(() => it, [|resourceState|]));

      // let ctx = Some({resourceState, fetchResources, getResource});

      Common.reactContextProvider(
        ~children,
        ~context=reactContext,
        ~value=ctx,
      );
    };
  };

  exception ResourceContextNotFound;

  let useResources = () => {
    switch (React.useContext(reactContext)) {
    | Some(ctx) => ctx
    | None => raise(ResourceContextNotFound)
    };
  };
};