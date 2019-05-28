open Store;

[@react.component]
let make = () => {
  <Store.ResourceContext.Provider>
    <Router />
  </Store.ResourceContext.Provider>;
};