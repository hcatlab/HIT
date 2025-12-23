# hit-web

> Elm Land SPA with Pico CSS

## Setup

Requires Node.js v18+ and backend running at `http://localhost:8080`

```bash
npm install
npm start  # dev server at :1234
```

## Commands

```bash
npm run build           # production build
npm run dev:elm-land    # Elm only
npm run dev:sass        # Sass only
npx elm-land add page /route  # new page
```

## Adding pages

Pages must use `Page.new` with signature `Shared.Model -> Route () -> Page Model Msg`:

```elm
page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new { init, update, subscriptions, view }
        |> Page.withLayout (\_ -> Layouts.Default {})
```

## Deploy

`npm run build` outputs to `dist/`. Deploy to Netlify, Vercel, or any static host.