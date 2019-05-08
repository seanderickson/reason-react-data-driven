open Belt;
open Common;
open Store;

module BMS = Belt.Map.String;

let mapUrlToRoute = (url: ReasonReact.Router.url) => 
  switch(url.path) {
    // | ["project"] => Project
    // | ["microscope"] => Microscope
    | ["resource"] => Resource;
    // | ["planned"] => Planned
    // | ["actual"] => Actual
    | _ => Resource
  };


let initialResourceState:resourceState = {
  resources: None,
  error: None,
  isLoading: true
  };

[@react.component]
let make = () => {

  // let (route, setRoute) = React.useState(
  //   ()=>mapUrlToRoute(ReasonReactRouter.dangerouslyGetInitialUrl()));
  let (route, setRoute) = React.useState(
    ()=>ReasonReactRouter.dangerouslyGetInitialUrl());
  
  // let (resourceState, setResourceState) = React.useState(
  //   ()=>initialResourceState);

  // let (projectState, setProjectState) = React.useState(
  //   ()=>initialProjectState);
  
  // let (msState, setMsState) = React.useState(
  //   ()=>initialMsState);

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
    // let watcherId = ReasonReactRouter.watchUrl(url=>setRoute( _ => url |> mapUrlToRoute));
    let watcherId = ReasonReactRouter.watchUrl(url=>setRoute( _ => url));
    Some(() => {
      Js.log("apply Effect");
      ReasonReactRouter.unwatchUrl(watcherId);
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
      <li key=resource.name ><Link href=resource.name selected=(testUrl(route, resource.name)) >(str(resource.title))</Link></li>
    })
    |> ReasonReact.array)
    </ul>
  };

  let {resourceState, fetchResources, setResources}: Store.ResourceContext.t = Store.ResourceContext.useResources();
  
    <div>
      // <button onClick=(_=>fetchResources()) >(str("Fetch Resources..."))</button>
      // <br/>
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
    (
      {
        switch(route.path) {
          // | Project =>
          //   <ProjectForm 
          //     handleSubmit=setProjectState
          //     key="project_form"
          //     initialState=projectState />
          // | Microscope => 
          //   <MicroscopeForm 
          //     handleSubmit=setMsState
          //     key="ms_form"
          //     initialState=msState
          //     dispatchModal />
          | ["resource",..._] =>
            <ResourceListing  />
          | _ => (str("Error, unknown route: " ++ (route.path |> Belt.List.toArray |> Js.Array.joinWith("/"))))
          // | Planned =>
          //   <SimpleForm 
          //     handleSubmit
          //     key="planned_form"
          //     initialState=self.state.values />
          // | Actual =>
          //   <SimpleForm 
          //     handleSubmit
          //     key="actual_form"
          //     initialState=self.state.values />
          // | Project => ReasonReact.null
          // | Microscope => ReasonReact.null
        }
      }
    )
      </div>
    </div>
  </div>
};
