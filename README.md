# `idris-tui`

This is my take on a framework for rich terminal applications.

List of known supported terminals:
- iTerm2
- foot
- kitty
- alacritty

List of unsupported terminals:
- intelliJ (not tested)
- tmux (no sync update, to investigate)
- VTE terminals (not tested)
- anything not mentioned above.

For sixel image support, there's a soft dependency on `chafa` (an apt
name).

## Design

This library is inspired mainly by Elm, but it's been heavily
influenced by feedback from the Idris compiler, as well as the Idris
community, which is most helpful.

The design is still in flux, but the goal is to allow for:

- suitable for rapid prototyping
- egonomic API
- soft landing for folks new to Idris
  - not requiring knowledge of too many concepts to get started.
  - build something interactive right away.
- explore programming with dependent types.

It's not quite there yet, except for that last one.

### MVC vs MVU

In classical MVC, a model is a stateful object. This is a
model-view-update approach.

The central notions are:

- *View*
- *Event*
- *Handler*
- *Component*

There are some libraries / embedded DSLs for writing views and
handlers.

Concretely, `View` is an *interface* which types can implement,
somewhat like `Show`. `Component` is a dependent record type, which
pairs an internal state type with a compatible handler. Then there is
a library of reusable components which work on values of the
`Component` type. Then there is `MainLoop`, which is all the machinery
required to run a component as a UI.

### Geometry

Working with raw indices is annoying. This library provides:

- `Pos`   A 2D Point
- `Area`  A 2D Size
- `Rect`  A 2D Rectangle

Various operations are implemented on these which allow shifting,
scaling, and subdivision.

### Painting

- `Context` models the state of the terminal window
- Uses types from `TUI.Geometry`
- While `View`s can paint anywhere they want, a `window` is supplied
  for reference. Views can manipulate this rectangle and hand it off
  to subviews.

In comparison to ncurses, this library does direct drawing to the
terminal. And so, there are no intermediate buffers. Moreover,
there is no optimization of screen updates.

Instead of optimizing updates, I use [Kitty's synchronous update
protocol]
(https://gitlab.com/gnachman/iterm2/-/wikis/synchronized-updates-spec).
This is protocol is ignored on unsupported terminals -- what you get
may or may not be usable.

I would like to support progressive enhancement, but I also want to
support things like sixel graphics. To do optimized updates on legacy
terminals, the library needs to buffer its output (in order to
calculate deltas). For sixel data this gets tricky.

I might include optional support for this style of rendering, if
there's demand for it, but I don't want to mandate it at the moment.

# Limitations and Roadmap

Most of the limitations are down to planned features.

## No Buffering

This is a design limitation. See previous section.

What you lose without buffering is direct support for overlapping
windows. More tragically, there is no support for *clipping*, so it's
up to application code not render out-of-bounds. This also makes makes
scrolling components tricky to implement. There may be some
terminal-specific hacks to explore here.

And so, the drawing model favors a left-to-right, top-to-bottom
drawing order, which is robust against the inability to clip painting
to a specific region of the screen.

Fortunately, this dovetails with structural recursion. See `*split`,
and `*divide` in `TUI.Geometry` module. Or see `pack*` and `paint*` in
`TUI.View`. If you use these handy routines, you're on the garden
path.

The upside is that, since any `View` has complete freedom, you can do
things with layout that would be difficult or impossible in other
frameworks.

## Limited Key Handling

There's rudimentary ANSI decoding for basic keys. It's good enough to
at least start coding interactive things that run in your terminal,
but it's not yet enough to write a full-fledged application.

## Event Types are Hard Coded

The existing event types are hard-coded into the library mainloop and
input shim. The reason for this is that I can't figure out the best
way to generalize event handling without breaking other features of
the API that I think I like..

## Events are Processed Externally

Right now I use a small python script to set up STDIN properly so that
you can detect individual keystrokes. This python script actually
sends event reports to Idris encoded as JSON packets.

I will likely retain this code, since it seems quite useful for
testing and debugging, if nothing else.

This is waiting on library support, which should be coming soon.

## Keyboard Input

I plan to support this standard for [unambigous keyboard
input](https://sw.kovidgoyal.net/kitty/keyboard-protocol/).

For now, if you want to send a literal escape, you must press escape
twice.

## Feature Detection

For the moment, there is no support for termcap, terminfo, ANSI query
strings, etc. Therefore, there is no mechanism to support graceful
degredation and / or fallback functionality. Supported terminals are
supported, all others are not.

## Localization, Internationalization, and Accessiblity

I hope to get to this at some point. And I aim to do a decent job when
I get to it. But it's only worth it to me if other people end up using
this, and it's just too soon to say.
