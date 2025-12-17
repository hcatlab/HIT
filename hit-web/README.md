# HIT Web

Single-page web application (Elm + Pico CSS)

## Prerequisites

- Node.js and npm

## Setup

```bash
npm install
```

## Develop

```bash
npm run dev
```

Visit http://localhost:8000.

## Build

Dev build:

```bash
npm run build
```

Production build:

```bash
npm run build:prod
```

## Structure

- `sass/` - SCSS stylesheets
- `src/` - Elm source code
- `css/` - Generated CSS (from SASS)
- `dist/` - Compiled JavaScript (from Elm)
