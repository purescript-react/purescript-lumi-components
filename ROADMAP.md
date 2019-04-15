# Goals and Roadmap

Lumi Components aims to encapsulate the low level design patterns and UI building blocks used at Lumi. This library can be thought of as a DSL for constructing business-specific components which follow Lumi's style guidelines. This idea will continue to influence the components we create and the way we structure their APIs.

For example, an `Input` component which just wraps an HTML input and passes through props like `className` and `style` is too low-level. It requires too much knowledge of both CSS and our style rules (such as padding in multiples of 8 pixels) to use. This slows initial development of new pages, clutters up the code, and creates inconsistencies in the UI.

"But I see those props on many of these components!"

Yes, this is definitely a work in progress. While this is a rough outline of our goal for a 1.0 release, we're still working out the best ways to accomplish these goals. Short term plans include:

- remove the use of `Nullable`
  - these APIs are a remnant of having ported some of these components from JavaScript
- remove `String` props where possible
  - props like `variant` should be full enums or removed and the component split into separate more specialized components
  - `content`, `child`, `text`, etc, props are generally better as `JSX` than strings because it's just about as opaque while being flexible enough to allow interesting component composition
- remove global CSS
  - the CSS should generate unique class names to prevent possible conflicts with UI elements that aren't a part of this library
  - should be more type safe, both in style definition and the attaching of styles to components

Longer term plans may include more DSLs similar to `Form` for use cases such as layout, or the ability to import the component library into Sketch.
