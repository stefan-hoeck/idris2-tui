# Design #

This library is inspired mainly by Elm, but it has started to look
more like the combination of
[VTY](https://github.com/jtdaugherty/vty) +
[Brick](https://github.com/jtdaugherty/brick), which are mature
Haskell libraries. This library is a is an independent exploration of
similar territory, in Idris.

There are many TUI libraries. This well-worn path is being followed in
pursuit of the following subgoals:
  - explore [dependent types](
	https://en.wikipedia.org/wiki/Dependent_type) by revisiting a
	familiar subject.
  - as a UI framework,
	- be more useful than `dialog` / `whiptail`, but simpler than
      `ncurses`.
	- emphasis is on efficient keyboard interaction
	- embrace modern vtes now, backward compatibility later.
	- strive for orthogonality and composition

## Overview

In classical MVC, a model is a stateful object. In contrast, this
library uses model-view-update. States are immutable, and updates are
expressed via pure functions.

The central notions are:

### *State*

The complete state of your application. Changes to state should be
expressed as pure functions.

### *View*

`View` is an interface for drawing to the screen. Types which
implement `View` are called *views*.

### *Event*

User input, such as a key press or mouse movement.

### *Handler*

A pure function which updates application state in response to an
event.

### *Component*

A high level notion which unifies *state*, *view*, and *handler* into
a single value that can be composed with other components.

## Events

Right now there is only support for events of type `TUI.Key`. I would
really like to allow support for events of arbitrary type, but am not
sure how to do it. I have tried a couple times, but it seems to really
pollute the API, or else lead to problems with type checking.

Note: If you think you can help with this, please open an issue and
explain your approach. I'm open to breaking changes that will remove
this limitation.

There's a `MainLoop` interface for handling events. The library
provides three mainloop implementations:

- `Base`: reads stdin directly using `getChar` from base.
- `InputShim`: decodes events as JSON records sent via stdin.
- `Default`: chooses between `Base` and `InputShim` at runtime.

### ANSI Key Sequences ###

One aspect of event handling I know how to support is ANSI key
sequence decoding. I just haven't got around to implementing the full
spec. Expect breaking changes to the `Key` type as I add support for
function, modifier keys, and other miscellaneous keys.

See also: this section in the [Roadmap](ROADMAP.md#kbd)

## Geometry ##

Working with raw row-major coordinates is annoying. This library
provides:

- `Pos`   A 2D Point
- `Area`  A 2D Size
- `Rect`  A 2D Rectangle

Various operations are implemented on these which allow shifting,
scaling, and subdivision, and painting using geometric values.

The math in this module is integer-based, and sometimes a little
confusing. We're working with character cells rather than geometric
points. If you're experiencing issues with layout, it's possible
there's an off-by-one issue in this library.

A `Rect` is a tuple of a Pos and an Area. Rect has a bunch of useful
methods, like `(.s)` which returns its south edge, or `(.sw)` which
returns its southwest corner.

## Painting ##

- `Context` models the state of the terminal window
- Uses types from `TUI.Geometry`

In comparison to ncurses and vty, this library does direct drawing to
the terminal. And so, there are no intermediate buffers. Moreover,
there is no optimization of screen updates.

### Synchronous Updates ###

Instead of optimizing updates, I use: [iTerm2's synchronous updates](
https://gitlab.com/gnachman/iterm2/-/wikis/synchronized-updates-spec
). Right now this is hard-coded, and there's no feature detection for
it. Consider support for synchronized updates a soft requirement of
the library for now.

The said protocol is ignored on unsupported terminals -- what you get
may or may not be usable: the more bytes written to standard out, the
more likely you will experience flicker. This is why libraries like
ncurses do differential updates.

### Low-Level Drawing ###

If all you want to do is low-level rendering, you can use the routines
in `Painting` to get the job done. Call `runRaw` from your application
main, passing in an explicit drawing function.

Painting is fundamentally procedural, and painting order matters!

## View ##

`View` is an interface that's like 2D version of `Show`. A view
knows how to paint itself into a terminal `Context`, in one of a fixed
number of `State`s, and within a given `Rect`.

That views stay within bounds is `*not*` enforced by this library.

`View` implementations should make a best effort to scale themselves
to fit the given window. Sometimes this isn't possible, or isn't
desired, and so you retain the freedom to color outside the
lines. Remember, drawing order matters. Use this to your advanage.

## Layout ##

The `Layout` module provides a family of functions for arranging
`View`s on screen. This is the easy way to create responsive-ish
designs.

## Components ##

A component compbines a `View`able state and a key event handler.

The library provides a family of components which implement common
keyboard UI patterns, as well as some higher-order components
(e.g. `Form`) which allow for composition of components.

You can also define your own custom components.

The main purpose of a `Component` is to produce a value of some
concrete type. So, for example, a confirmation dialog might yield a
`Bool`, while a spinner might yield a `String` or an element of a
finite set, while a A form component might construct an entire record.

Components exist for:
- entering data
- choosing items from a list
- managing lists of values

Some higher order components take a user-provided key handler, or may
broadcast key vents to multiple subcomponents. In such cases, one
needs to be aware of the the possibility of key collisions, and either
guard against it, or exploit it.

Components can also compose as *modally*, via the
semantically-blocking `push` / `Push` response.
