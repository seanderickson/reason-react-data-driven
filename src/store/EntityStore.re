[%%debugger.chrome];
/**
  A React context (store) for Entity data to keep for global access:
  - store: hash from entity_key->entity_id->json data
  - implemented using an immutable map(string, map(string, json))

  React context design is based on these examples:
    https://kentcdodds.com/blog/application-state-management-with-react
  - and this adaptation for reason-react:
    https://github.com/mixvar/reason-react-context-playground
 */
open Belt;
open Metadata;
open EntityModules;

let debug_mode = false;

// entityType: one for each API entity;
// use a polymorphic variant: the jsConverter will generate the
// "entityTypeFromJs" and "entityTypeToJs" functions.
// TODO: Not complete listing of resources
[@bs.deriving jsConverter]
type entityType = [ | `microscope | `person | `irb | `project | `experiment];

// Note: jsConverter with ordinary variants translates to integer values:
// [@bs.deriving jsConverter]
// type entityType1 =
//   | Microscope
//   | Person;
// Js.log2("Microscope: ", entityType1ToJs(Microscope)); // "Microscope: 0"

type webLoadingData('a) =
  | NotAsked
  | Loading(entityType)
  | LoadFailure(entityType, string)
  | LoadSuccess(entityType, 'a);

module ResourceContext = {
  type webLoadingState = webLoadingData(option(array(Js.Json.t)));
  type store = Belt.Map.String.t(Belt.Map.String.t(Js.Json.t));
  type entityStoreState = {
    webLoadingState,
    store,
  };

  type t = {
    entityStoreState,
    fetchEntities: entityType => Js.Promise.t(unit),
    getEntity: (entityType, string) => option(Js.Json.t),
    getFilledResource: string => option(Resource.t),
  };

  let reactContext = React.createContext(None);

  module Provider = {
    [@react.component]
    let make = (~children) => {
      let {resourceState, fetchResources, getResources, getResource}: Store.ResourceContext.t =
        Store.ResourceContext.useResources();

      Js.log2("EntityStore initial resourceState: ", resourceState);

      let getId = (entityType, json) =>
        switch (entityType) {
        | `microscope => Microscope.getId(json)
        | `person => Person.getId(json)
        | `irb => Irb.getId(json)
        | `project => Project.getId(json)
        | `experiment => Experiment.getId(json)
        };

      let (entityStoreState, dispatchState) =
        React.useReducer(
          (state, action) =>
            switch (action) {
            | NotAsked => {
                webLoadingState: NotAsked,
                store: Belt.Map.String.empty,
              }
            | LoadSuccess(entityType, optionalArray) =>
              switch (optionalArray) {
              | Some(entityList) => {
                  webLoadingState: LoadSuccess(entityType, optionalArray),
                  store:
                    Belt.Map.String.set(
                      state.store,
                      entityTypeToJs(entityType),
                      Belt.Map.String.fromArray(
                        Belt.Array.map(entityList, json =>
                          (getId(entityType, json), json)
                        ),
                      ),
                    ),
                }
              | None => {
                  webLoadingState: LoadSuccess(entityType, optionalArray),
                  store: Belt.Map.String.empty,
                }
              }
            | LoadFailure(entityType, message) => {
                webLoadingState: LoadFailure(entityType, message),
                store:
                  Belt.Map.String.set(
                    state.store,
                    entityTypeToJs(entityType),
                    Belt.Map.String.empty,
                  ),
              }
            | Loading(entityType) => {
                ...state,
                webLoadingState: Loading(entityType),
              }
            },
          {webLoadingState: NotAsked, store: Belt.Map.String.empty},
        );

      let fetchEntities = entityType => {
        let entityName = entityTypeToJs(entityType);
        dispatchState(Loading(entityType));
        ApiClient.getEntityListing(entityName)
        |> Js.Promise.then_(result => {
             switch (result) {
             | Result.Ok(entityListing) =>
               dispatchState(LoadSuccess(entityType, Some(entityListing)));
               Js.Promise.resolve();
             | Result.Error(message) =>
               dispatchState(LoadFailure(entityType, message));
               Js.Promise.resolve();
             };
             Js.Promise.resolve();
           });
      };

      let fetchDefaultEntities = () => {
        let entities = [|`person, `microscope, `irb, `project|];
        Js.Promise.all(
          entities |> Js.Array.map(etype => fetchEntities(etype)),
        )
        |> ignore;
      };

      let getEntities = entityType =>
        Belt.Map.String.get(
          entityStoreState.store,
          entityTypeToJs(entityType),
        );

      let getEntity = (entityType, entityId) => {
        switch (getEntities(entityType)) {
        | Some(entityMap) => Belt.Map.String.get(entityMap, entityId)
        | None => None
        };
      };

      let getUserVocabulary = () => {
        Belt.Option.map(getEntities(`person), entityMap =>
          Belt.Map.String.valuesToArray(entityMap)
          |> Belt.Array.map(_, (json) =>
               (
                 {
                   let p = Person.decode(json);
                   {
                     scope: "user.vocab",
                     key: p.id |> string_of_int,
                     title: p.first_name ++ " " ++ p.last_name,
                     description: None,
                   };
                 }: Vocabulary.t
               )
             )
        );
      };

      let getAutosuggestScientistVocabulary = () => {
        Js.log("getAutosuggestScientistVocabulary...");
        let field: Field.t =
          getResource("project")
          |> Belt.Option.flatMap(_, r =>
               Resource.getField(r, "scientist_samples")
             )
          |> Belt.Option.getExn;
        let result =
          Belt.Option.map(getEntities(`project), entityMap =>
            Belt.Map.String.valuesToArray(entityMap)
            |> Belt.Array.map(_, json =>
                 Metadata.Field.getDisplayValue(json, field)
               )
            |> Belt.Array.keepMap(_, a => a)
            |> Belt.Set.String.fromArray
            |> Belt.Set.String.toArray
            |> Belt.Array.map(
                 _,
                 scientist_samples => {
                   let r: Vocabulary.t = {
                     scope: "autosuggest.scientist",
                     key: scientist_samples,
                     title: scientist_samples,
                     description: None,
                   };
                   r;
                 },
               )
          );
        Js.log2("result", result);
        result;
      };

      let getMicroscopeVocabulary = () => {
        Js.log("getMicroscopeVocabulary...");
        Belt.Option.map(getEntities(`microscope), entityMap =>
          Belt.Map.String.valuesToArray(entityMap)
          |> Belt.Array.map(_, (json) =>
               (
                 {
                   let microscope = Microscope.decode(json);
                   {
                     scope: "microscope.name",
                     key: microscope.id |> string_of_int,
                     title: microscope.name,
                     description:
                       Some(Js.Array.joinWith(", ", microscope.objectives)),
                   };
                 }: Vocabulary.t
               )
             )
        );
      };

      let getIrbVocabulary = () => {
        Belt.Option.map(getEntities(`irb), entityMap =>
          Belt.Map.String.valuesToArray(entityMap)
          |> Belt.Array.map(_, (json) =>
               (
                 {
                   let irb = Irb.decode(json);
                   {
                     scope: "irb.vocab",
                     key: irb.id |> string_of_int,
                     title: irb.irb_id ++ " - " ++ irb.type_,
                     description: None,
                   };
                 }: Vocabulary.t
               )
             )
        );
      };

      // Update Resource data with vocabularies for person, irb, etc.
      let getFilledResource = entityName => {
        Js.log2("getFilledResource: ", entityName);
        getResource(entityName)
        |> Belt.Option.map(_, originalResource =>
             {
               ...originalResource,
               fields:
                 Belt.Array.map(
                   originalResource.fields,
                   field => {
                     if (debug_mode) {
                       Js.log3("fill field", entityName, field.name);
                     };
                     switch (field.vocab_scope) {
                     | Some(vocab_scope) =>
                       switch (vocab_scope) {
                       | "person.id" => {
                           ...field,
                           vocabularies: getUserVocabulary(),
                         }
                       | "irb.id" => {
                           ...field,
                           vocabularies: getIrbVocabulary(),
                         }
                       | "autosuggest.scientist" => {
                           ...field,
                           vocabularies: getAutosuggestScientistVocabulary(),
                         }
                       | "microscope.name" => {
                           ...field,
                           vocabularies: getMicroscopeVocabulary(),
                         }
                       | _ => field
                       }
                     | _ => field
                     };
                   },
                 ),
             }
           );
      };

      React.useEffect0(() => {
        Js.log("Initial fetch...");
        fetchDefaultEntities();
        Some(() => Js.log("cleanup Effect"));
      });

      // Memoize because of dependency on resourceState
      let ctx =
        Some({entityStoreState, fetchEntities, getEntity, getFilledResource});
      // let ctx: option(t) =
      //   Some({entityStoreState, fetchEntities, getEntity, getFilledResource})
      //   |> (it => React.useMemo1(() => it, [|resourceState|]));

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