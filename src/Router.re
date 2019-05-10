open Belt;
open Common;
open Store;

[@react.component]
let make = () => {

  let route = ReasonReactRouter.useUrl();
  // let (route, setRoute) = React.useState(
  //   ()=>ReasonReactRouter.dangerouslyGetInitialUrl());

  Js.log2("route:", route);

  let (modalState, dispatchModal) = React.useReducer(
    (state, action)=>
      switch(action) {
        | Hide => { ...state, shown: false}
        | Show(message, callBackOk, callBackCancel) =>
            { message, shown: true, callBackOk, callBackCancel }
        | ModalCancel => {
            state.callBackCancel("Cancel");
            { ...state, shown: false };
          }
        | ModalOk => {
            state.callBackOk("ok");
            {...state, shown: false };
        }
      },
      {
        message: "", shown: false, 
        callBackOk: (_)=>(), callBackCancel: (_)=>()
      }
  );

  React.useEffect(() => {
    // let watcherId = ReasonReactRouter.watchUrl(url=>setRoute( _ => url));
    Some(() => {
      Js.log("apply Effect");
      // ReasonReactRouter.unwatchUrl(watcherId);
    });
  });

  let testUrl =  (url: ReasonReact.Router.url, test) => 
    switch(url.path) {
      |  [testval, ..._] => testval==test
      | _ => false
    };

  let printResourceMenu = resources => {
    <ul>
    (resources
    |> Array.map(_, resource => {
      <li key=resource.name ><Link href=("/" ++ resource.name) selected=(testUrl(route, resource.name)) >(str(resource.title))</Link></li>
    })
    |> ReasonReact.array)
    </ul>
  };

  let {resourceState, fetchResources, setResources}: Store.ResourceContext.t = Store.ResourceContext.useResources();
  
  let getResource = (resourceName) => switch(resourceState.resources) {
      | Some(resources) => resources -> Belt.Array.getBy(resource => resource.name==resourceName)
      | None => None
    };

    <div>
      <Modal 
        dispatchModal
        customClass="foo" 
        show=modalState.shown >
        (str(modalState.message))
      </Modal>
      <div className="wrapper">
        (switch(resourceState.resources) {
          | Some(resources) => resources |> printResourceMenu
          | None => ReasonReact.null
        })      
        <div className="content">
      {
        Js.log2("Path: ", route.path);
        switch(route.path) {
          | ["resource",..._] =>
            <ResourceListing  />
          | [resourceName] => {
            let foundResource = getResource(resourceName);
            switch(foundResource){
              | Some(resource) => <EntityListing key=resource.name resource />
              | None => (str("Unknown resource: " ++ resourceName))
            }
          }
          | [resourceName, entityId] => {
            let foundResource = getResource(resourceName);
            switch(foundResource){
              | Some(resource) =><DetailView key=(resource.name ++ ":" ++ entityId) resource id=entityId />
              | None => (str("Unknown resource: " ++ resourceName))
            }
          }
          | _ => (str("Error, unknown route: " ++ (route.path |> Belt.List.toArray |> Js.Array.joinWith("/"))))
        }
      }
      </div>
    </div>
  </div>
};
