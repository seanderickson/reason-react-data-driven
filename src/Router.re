open Belt;
open Common;
open Metadata;
open Store;

[@react.component]
let make = () => {
  let route = ReasonReactRouter.useUrl();

  Js.log2("Route: ", route);

  // Modal state

  let (modalState, dispatchModal) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | Hide => {...state, shown: false}
        | Show(message, callBackOk, callBackCancel) => {
            message,
            shown: true,
            callBackOk,
            callBackCancel,
          }
        | ModalCancel =>
          state.callBackCancel("Cancel");
          {...state, shown: false};
        | ModalOk =>
          state.callBackOk("ok");
          {...state, shown: false};
        },
      {
        message: "",
        shown: false,
        callBackOk: _ => (),
        callBackCancel: _ => (),
      },
    );

  // Store (resources) state
  let {userContextState, logIn, logOut, getUser}: LoginContext.ResourceContext.t =
    LoginContext.ResourceContext.useResources();

  let {resourceState, fetchResources, getResource}: Store.ResourceContext.t =
    Store.ResourceContext.useResources();
  let {entityStoreState, fetchEntities, getEntity, getFilledResource}: EntityStore.ResourceContext.t =
    EntityStore.ResourceContext.useResources();

  // DOM methods
  let printTitle = (resource: Resource.t, entityId: string) => {
    <div
      key={"resource-title-" ++ resource.name}
      className="border-solid border-2 border-gray-300 text-center rounded m-2">
      {str(resource.title ++ ": " ++ entityId)}
    </div>;
  };

  let printResourceMenu = (resources: Resource.resources) => {
    let testUrl = (url: ReasonReact.Router.url, test) =>
      switch (url.path) {
      | [testval, ..._] => testval == test
      | _ => false
      };

    let example: Metadata.Resource.t = {
      id: 999,
      name: "examples",
      title: "Examples",
      description: "For testing",
      fields: [||],
      resource_ref: None,
    };
    // let exampleForLogin: Metadata.Resource.t = {
    //   id: 999,
    //   name: "login",
    //   title: "RT-Login",
    //   description: "For testing: placeholder for menu",
    //   fields: [||],
    // };
    <ul className="m-2" key="resources">
      {resources
       |> Array.concat(_, [|example|])
       |> Array.map(_, resource =>
            <li key={resource.name}>
              <Link
                href={"/" ++ resource.name}
                selected={testUrl(route, resource.name)}>
                {str(resource.title)}
              </Link>
            </li>
          )
       |> ReasonReact.array}
    </ul>;
  };

  let printContent = () => {
    <div key="content" className="content">
      {switch (route.path) {
       | [] => ReasonReact.null
       | [resourceName] =>
         switch (resourceName) {
         | "examples" => <ExamplesView />
         | _ =>
           switch (getFilledResource(resourceName)) {
           | Some(resource) =>
             switch (resourceName) {
             | "canonical"
             | "search" =>
               //  Js.log2("route.search: ", route.search);
               let defaultSearchValue =
                 decodeURIComponent(route.search)
                 |> Js.String.split("=")
                 |> Belt.Array.get(_, 1)
                 |> Belt.Option.getWithDefault(_, route.search)
                 |> Js.String.trim;

               let initialState =
                 Js.String.length(defaultSearchValue) > 0
                   ? RtSearchView.Searching(defaultSearchValue)
                   : RtSearchView.NotAsked;
               //  <RtSearchView
               //    resource
               //   //  initialState=RtSearchView.NotAsked
               //    initialSearch={Some(defaultSearchValue)}
               //  />;
               // <RtSearchView2
               //   resource
               //   initialSearch={Some(defaultSearchValue)}
               // />;
               <RtSearchView3
                 resource
                 initialSearch={
                   Js.String.length(defaultSearchValue) > 0
                     ? Some(defaultSearchValue) : None
                 }
               />;
             | _ => <EntityListingTable key={resource.name} resource />
             }

           | None => str("Unknown resource: " ++ resourceName)
           }
         }
       | [resourceName, entityId, ...tail] =>
         let foundResource = getFilledResource(resourceName);
         switch (foundResource) {
         | Some(resource) =>
           <>
             {printTitle(resource, entityId)}
             {switch (resource.name) {
              | "project" =>
                <ProjectView
                  resource
                  projectId=entityId
                  urlStack=tail
                  dispatchModal
                />
              | "experiment" =>
                <ExperimentView
                  resource
                  entityId
                  urlStack=tail
                  dispatchModal
                />
              //  | "search" => <RTReagentView resource defaultSearchValue=entityId />
              | _ =>
                <EntityView
                  key={resource.name ++ "/" ++ entityId}
                  resource
                  entityId
                  urlStack=tail
                />
              }}
           </>
         | None => str("Unknown resource: " ++ resourceName)
         };
       }}
    </div>;
  };

  let renderContent = () => {
    <div>
      <Modal dispatchModal customClass="foo" show={modalState.shown}>
        {str(modalState.message)}
      </Modal>
      <div className="wrapper">
        {switch (resourceState) {
         | NotAsked => str("Not asked...")
         | LoadFailure(msg) => str("Load failure: " ++ msg)
         | Loading => str("Loading...")
         | LoadSuccess(resources) =>
           switch (entityStoreState.webLoadingState) {
           | LoadSuccess(_, _) =>
             switch (resources) {
             | Some(resources) =>
               [|printResourceMenu(resources), printContent()|]
               |> ReasonReact.array
             | None => str("No resources found")
             }
           | NotAsked => str("Not asked...")
           | LoadFailure(entityType, msg) =>
             str(
               "Load failure: "
               ++ EntityStore.entityTypeToJs(entityType)
               ++ ": "
               ++ msg,
             )
           | Loading(entityType) =>
             str("Loading: " ++ EntityStore.entityTypeToJs(entityType))
           }
         }}
      </div>
    </div>;
  };

  <div>
    <div
      className="text-center border-solid border-2 border-gray-300 rounded m-2">
      <div className=" "> {str("Cycif Prototype")} </div>
      {switch (userContextState.loginStatus) {
       | LoginSuccess(user) =>
         <div
           style={ReactDOMRe.Style.make(
             ~position="absolute",
             ~top="10px",
             ~right="20px",
             (),
           )}
           className="">
           {str("Logged in as:" ++ nbsp ++ user.username ++ nbsp)}
           <a
             href="#"
             onClick={evt =>
               // ReactEvent.Mouse.preventDefault(evt);
               logOut()}>
             {str("Log out")}
           </a>
         </div>
       | _ => ReasonReact.null
       }}
    </div>
    {switch (userContextState.loginStatus) {
     | NotLoggedIn => <Login loginHandler=logIn />
     | Loading => str("Logging in")
     | LoginSuccess(_user) => renderContent()
     | LoginFail(msg) => str("Failed login: " ++ msg)
     }}
  </div>;
};