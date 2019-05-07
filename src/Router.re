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

  let (route, setRoute) = React.useState(
    ()=>mapUrlToRoute(ReasonReactRouter.dangerouslyGetInitialUrl()));
  
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

  // let fetchResources = (event) => {
  //   Js.log("handle click...");
  //   ApiClient.getResources()
  //   |> Js.Promise.then_(result => {
  //       switch (result) {
  //       | Result.Ok(resources) => setResourceState(_=>{...initialResourceState, resources: Some(resources)})
  //       | Result.Error(message) => setResourceState(_=>{...initialResourceState, error: Some(message)})
  //       };
  //       Js.Promise.resolve();
  //     })
  //   |> ignore;
  // };

  // let {resourceState, fetchResources, setResources}: Store.ResourceContext.t = Store.ResourceContext.useResources();

  React.useEffect(() => {
    let watcherId = ReasonReactRouter.watchUrl(url=>setRoute( _ => url |> mapUrlToRoute));
    // if (route == Resource){
    //   // get route

    // }
    Some(() => {
      Js.log("apply Effect");
      ReasonReactRouter.unwatchUrl(watcherId);
    });
  });

  <Store.ResourceContext.Provider>
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
    <div>
    <ul>
      // <li><Link href="project" selected=((route == Project)) >(str("project"))</Link></li>
      // <li><Link href="microscope" selected=((route == Microscope)) >(str("microscope"))</Link></li>
      <li><Link href="resource" selected=((route == Resource)) >(str("Resources"))</Link></li>
    </ul>
    </div>
    (
      {
        switch(route) {
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
          | Resource =>
            <ResourceListing  />
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
          | Project => ReasonReact.null
          | Microscope => ReasonReact.null
        }
      }
    )
  </div>
  </div>
  </Store.ResourceContext.Provider>;
};
