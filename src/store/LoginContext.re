[%%debugger.chrome];
/**
  A React context (store) for user login data.

  React context design is based on these examples:
    https://kentcdodds.com/blog/application-state-management-with-react
  - and this adaptation for reason-react:
    https://github.com/mixvar/reason-react-context-playground
 */

let debug_mode = false;

// Js.Json.object_
module ApiUser = {
  type t = {
    id: int,
    username: string,
    full_name: string,
    email: string,
    roles: array(string),
  };

  let decode = json => {
    Json.Decode.{
      id: json |> field("id", int),
      username: json |> field("username", string),
      full_name: json |> field("full_name", string),
      email: json |> field("email", string),
      roles: json |> field("roles", array(string)),
    };
  };

  let encode = (username, password) => {
    Json.Encode.(
      object_([
        ("username", string(username)),
        ("password", string(password)),
      ])
    );
  };

  let sessionDecoder =
    Json.Decode.field("session", Json.Decode.field("user", decode));
};

type loginStatus =
  | NotLoggedIn
  | Loading
  | LoginSuccess(ApiUser.t)
  | LoginFail(string);

module ResourceContext = {
  type userContextState = {
    loginStatus,
    user: option(ApiUser.t),
  };

  type t = {
    userContextState,
    logIn: (string, string) => Js.Promise.t(unit),
    // logIn: (string, string) => unit,
    logOut: unit => unit,
    getUser: unit => option(ApiUser.t),
  };

  let reactContext = React.createContext(None);

  module Provider = {
    [@react.component]
    let make = (~children) => {
      let (userContextState, dispatchState) =
        React.useReducer(
          (state, action) =>
            switch (action) {
            | NotLoggedIn => {loginStatus: NotLoggedIn, user: None}
            | Loading => {...state, loginStatus: Loading}
            | LoginSuccess(user) => {
                loginStatus: LoginSuccess(user),
                user: Some(user),
              }
            | LoginFail(msg) => {loginStatus: LoginFail(msg), user: None}
            },
          {loginStatus: NotLoggedIn, user: None},
        );

      let getCookie: unit => Js.Nullable.t(string) = [%bs.raw
        {|
          function () {
            var COOKIE_NAME = 'auth_tkt';
            // var COOKIE_NAME = 'csrftoken';
            // var COOKIE_PATTERN = new RegExp( COOKIE_NAME + '=([^;]+)', 'i' );
            var COOKIE_PATTERN = new RegExp( COOKIE_NAME + '=(.*)', 'i' );
            var match = document.cookie.match( COOKIE_PATTERN );
            if (match) {
              return match[1];
            }
            return null;
          }
        |}
      ];

      let logIn = (username, password) => {
        Js.log2("log in: ", username);
        let apiUrl = "/api/v0/login";
        Js.Promise.(
          Fetch.fetchWithInit(
            apiUrl,
            Fetch.RequestInit.make(
              ~method_=Fetch.Post,
              ~body=
                ApiUser.encode(username, password)
                |> Js.Json.stringify
                |> Fetch.BodyInit.make,
              ~headers=
                Fetch.HeadersInit.make({"Content-Type": "application/json"}),
              (),
            ),
          )
          |> then_(response => {
               let status = response->Fetch.Response.status;
               let statusText = response->Fetch.Response.statusText;
               if (!response->Fetch.Response.ok) {
                 let errMsg = {j|Response Error: status=$status, "$statusText" |j};
                 dispatchState(LoginFail(errMsg));
                 resolve();
               } else {
                 Js.log("Status ok, decoding json...");
                 response
                 |> Fetch.Response.json
                 |> then_(json => {
                      Js.log2("LoginSuccess", json);
                      dispatchState(
                        LoginSuccess(ApiUser.sessionDecoder(json)),
                      );
                      resolve();
                    });
               };
             })
          |> catch(err => {
               Js.log2("post error", err);
               dispatchState(
                 LoginFail({j|API POST error(URL: $apiUrl, error=$err)|j}),
               );
               resolve();
             })
        );
      };

      let logOut = () => {
        dispatchState(NotLoggedIn);
        // let apiUrl = "http://localhost:8000/api/v0/logout";
        let apiUrl = "/api/v0/logout";
        Js.Promise.(
          Fetch.fetchWithInit(
            apiUrl,
            Fetch.RequestInit.make(
              ~method_=Fetch.Post,
              ~headers=
                Fetch.HeadersInit.make({"Content-Type": "application/json"}),
              (),
            ),
          )
          |> then_(response => {
               Js.log2("process response", response);
               response
               |> Fetch.Response.json
               |> then_(json => {
                    Js.log2("LogoutSuccess", json);
                    resolve();
                  });
             })
          |> catch(err => {
               Js.log2("post error", err);
               //  dispatchState(
               //    LogoutFail({j|API POST error(URL: $apiUrl, error=$err)|j}),
               //  );
               resolve();
             })
        )
        |> ignore;
      };

      let checkSession = () => {
        Js.log("userContext checkSession...");
        // let apiUrl = "http://localhost:8000/api/v0/session";
        let apiUrl = "/api/v0/session";
        Js.Promise.(
          Fetch.fetchWithInit(
            apiUrl,
            Fetch.RequestInit.make(
              ~method_=Fetch.Get,
              ~headers=
                Fetch.HeadersInit.make({"Content-Type": "application/json"}),
              (),
            ),
          )
          |> then_(response =>
               switch (response->Fetch.Response.status) {
               | 403 =>
                 dispatchState(NotLoggedIn);
                 resolve();
               | 200 =>
                 response
                 |> Fetch.Response.json
                 |> then_(json => {
                      Js.log2("LoginSuccess", json);
                      dispatchState(
                        LoginSuccess(ApiUser.sessionDecoder(json)),
                      );
                      resolve();
                    })
               | otherStatus =>
                 let statusMsg = response->Fetch.Response.statusText;
                 Js.log3("other status:", otherStatus, statusMsg);
                 dispatchState(
                   LoginFail(
                     {j|Bad status from server: $otherStatus, $statusMsg |j},
                   ),
                 );
                 resolve();
               }
             )
          |> catch(err => {
               Js.log2("login session error", err);
               dispatchState(
                 LoginFail({j|API Session error(URL: $apiUrl, error=$err)|j}),
               );
               resolve();
             })
        )
        |> ignore;
      };

      let getUser = () => {
        Js.log("TODO: getUser...");
        None;
      };

      React.useEffect0(() => {
        if (userContextState.loginStatus == NotLoggedIn) {
          checkSession();
        };
        Some(() => Js.log("cleanup Effect"));
      });

      let ctx = Some({userContextState, logIn, logOut, getUser});
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