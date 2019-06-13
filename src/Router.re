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

  let {resourceState, fetchResources, getResource}: Store.ResourceContext.t =
    Store.ResourceContext.useResources();

  // DOM methods

  let printResourceMenu = (resources:Resource.resources) => {
    let testUrl = (url: ReasonReact.Router.url, test) =>
      switch (url.path) {
      | [testval, ..._] => testval == test
      | _ => false
      };

    <ul>
      {resources
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
         switch (resources) {
         | Some(resources) => resources->printResourceMenu
         | None => str("No resources found")
         }
       }}
      <div className="content">
        {switch (route.path) {
         | [] => ReasonReact.null
         | [resourceName] =>
           let foundResource = getResource(resourceName);
           switch (foundResource) {
           | Some(resource) =>
             <EntityListingTable key={resource.name} resource />
           | None => str("Unknown resource: " ++ resourceName)
           };
         | [resourceName, entityId, ...tail] =>
           let foundResource = getResource(resourceName);
           switch (foundResource) {
           | Some(resource) =>
             if (resourceName == "project") {
               <ProjectView
                 resource
                 projectId=entityId
                 urlStack=tail
                 dispatchModal
               />;
             } else {
               <EntityView
                 key={resource.name ++ "/" ++ entityId}
                 resource
                 entityId
                 urlStack=tail
               />;
             }
           | None => str("Unknown resource: " ++ resourceName)
           };
         }}
      </div>
    </div>
  </div>;
};