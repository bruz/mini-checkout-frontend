# Mini Checkout - frontend

This is the frontend for a simple checkout tracking web app. It was mainly an exercise to learn Elm, but is used by a local nonprofit to track sales for an event they run.

See the backend at https://github.com/bruz/mini-checkout-backend.

## Install

Prerequisites:

* [Node.js](https://nodejs.org/en/download/)
* [Elm](https://guide.elm-lang.org/install.html)
* [Mini Checkout backend](https://github.com/bruz/mini-checkout-backend)

Setup:

```bash
git clone https://github.com/bruz/mini-checkout-frontend
cd mini-checkout-frontend
elm-package install
npm install
```

## Development server

```bash
npm start
```

The app should be available at http://localhost:8080 (assuming the backend is running).

## Deploy to Heroku

This uses two buildpacks, one to build the static assets, and another to serve them with proxying to the API and basic auth.

1. Deploy the backend (see docs for it)
2. Create a `static.json` based on `static.json.template`, substituting your actual API address.
3. Configure environment variables and buildpacks

```bash
heroku buildpacks:add heroku/nodejs
heroku buildpacks:add https://github.com/doinginc/heroku-buildpack-static.git#basic-auth
heroku config:set API_BASE_URL=/api/ BASIC_AUTH_USERNAME=YOUR-USERNAME BASIC_AUTH_PASSWORD=$(openssl passwd -apr1 YOUR-PASSWORD)
```

4. Deploy!
