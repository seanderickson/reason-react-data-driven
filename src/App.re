open Common;

[@react.component]
let make = () => {
  <LoginContext.ResourceContext.Provider>
    <Store.ResourceContext.Provider>
      <EntityStore.ResourceContext.Provider>
        <Router />
      </EntityStore.ResourceContext.Provider>
    </Store.ResourceContext.Provider>
  </LoginContext.ResourceContext.Provider>;
};