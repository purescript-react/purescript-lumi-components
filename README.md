# purescript-lumi-components [![Build Status](https://travis-ci.org/lumihq/purescript-lumi-components.svg?branch=master)](https://travis-ci.org/lumihq/purescript-lumi-components)

[lumihq.github.io/purescript-lumi-components](https://lumihq.github.io/purescript-lumi-components/)

This is a component library focused on Lumi's specific UI and UX needs. Available components are found in `src/components`.

## Goals and Roadmap

See [ROADMAP.md](ROADMAP.md) for more info.

## Installation

```sh
bower i -S purescript-lumi-components
```

To use the styles that come with these components the CSS needs to be injected into the page. The easiest way to do this is to run the `attachGlobalComponentStyles` effect available in `Lumi.Components.Styles` one time as your application initializes.

You will also need a few `npm` dependencies. These dependencies and their versions must be compatible with the ones listed [here](https://github.com/lumihq/purescript-lumi-components/blob/master/package.json#L30).

## Local development

```sh
npm i; npx bower i; npx pulp build
npm start
```

You can run production builds (output minified, static files to `build/`) using `npm run build`.


## Tagging a new release

```sh
pulp version 0.x.y
git push --follow-tags
npm run deploy
```

_Don't use `npm version`!_

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for more info.
