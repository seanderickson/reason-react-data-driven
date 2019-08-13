# cycif-reason-proto

A simple example that uses React context to handle Fetch / Json Decoding with promises.
- use a mock db with some of the Cycif domain objects,
- implement a simple resource/field schema to enable parsing and display.

## Run Project

```sh
npm install
npm start
# in another tab
npm run webpack (for index.html)
or
npm run server (for dev server at localhost:8000)
```

**Started with**, bsb -init my-react-app -theme react-hooks

## Run Project with Server

To run with the webpack development server run `npm run server` and view in the browser at http://localhost:8000. Running in this environment provides hot reloading and support for routing; just edit and save the file and the browser will automatically refresh.

Note that any hot reload on a route will fall back to the root (`/`), so `ReasonReact.Router.dangerouslyGetInitialUrl` will likely be needed alongside the `ReasonReact.Router.watchUrl` logic to handle routing correctly on hot reload refreshes or simply opening the app at a URL that is not the root.

To use a port other than 8000 set the `PORT` environment variable (`PORT=8080 npm run server`).

## Generate the metadata for the site ##

$ source [virtualenv]

$ PYTHONPATH=. python utils/metadata_writer.py -f metadata.json.xlsx -of metadata.json -v

## Run the "REST" API test server ##

$ json-server --watch metadata.json 

## Build for Production

```sh
npm run clean
npm run build
npm run webpack:production
```

This will replace the development artifact `build/Index.js` for an optimized version as well as copy `src/index.html` into `build/`. You can then deploy the contents of the `build` directory (`index.html` and `Index.js`).

If you make use of routing (via `ReasonReact.Router` or similar logic) ensure that server-side routing handles your routes or that 404's are directed back to `index.html` (which is how the dev server is set up).

**To enable dead code elimination**, change `bsconfig.json`'s `package-specs` `module` from `"commonjs"` to `"es6"`. Then re-run the above 2 commands. This will allow Webpack to remove unused code.
