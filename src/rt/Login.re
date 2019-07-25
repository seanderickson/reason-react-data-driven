open Common;

[@react.component]
let make = (~loginHandler) => {
  let loginAction = event => {
    ReactEvent.Synthetic.preventDefault(event);
    let domForm = ReactEvent.Form.target(event);
    let username = domForm##elements##loginUsername##value;
    loginHandler(username, domForm##elements##loginPassword##value);
    ();
  };

  <div className="wrapper">
    <div id="left" />
    <div id="right">
      <form className="detail_table" onSubmit=loginAction>
        <div className="detail_table_row">
          <div className="md:text-right">
            <label htmlFor="loginUsername" className="font-bold text-right">
              {str("Username")}
            </label>
          </div>
          <input
            type_="username"
            placeholder="Username"
            id="loginUsername"
            className="appearance-none border-2 rounded max-w-md w-full py-1 leading-tight"
          />
        </div>
        <div className="detail_table_row">
          <div className="md:text-right">
            <label htmlFor="loginPassword" className="font-bold text-right">
              {str("Password")}
            </label>
          </div>
          <input
            type_="password"
            placeholder="Password"
            id="loginPassword"
            className="appearance-none border-2 rounded max-w-md w-full py-1 leading-tight"
          />
        </div>
        <div className="detail_table_row">
          <div className="col1">
            <div className="checkbox">
              <label title="">
                <input type_="checkbox" value="on" />
                {str("Remember me")}
              </label>
            </div>
          </div>
        </div>
        <div className="detail_table_row">
          <button type_="submit" className="btn btn-gray">
            {str("Sign in")}
          </button>
        </div>
      </form>
    </div>
  </div>;
};