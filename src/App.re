
[@react.component]
let make = () => {
  <Store.ResourceContext.Provider>
    <EntityStore.ResourceContext.Provider>
      <Router />
    </EntityStore.ResourceContext.Provider>
  </Store.ResourceContext.Provider>;
};
