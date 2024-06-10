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

This library is inspired mainly by the Elm / React model-view-update
pattern, but adapted for keyboard-orientation, and returning somewhat
to the original notion of MVC.

### MVC

- A `Model` knows how to update itself in response to some `Action`.
- A `View` knows how to paint itself to the screen.
- A `Controller` decides what actions to perform in response to events.

These concepts are all expressed as interfaces in this framework. When
the same `stateT` implements all three interfaces, it becomes a
`Component`. There is a family of stock components in the
`TUI.Component` module.

All four interfaces can be composed in various ways, and various entry
points are provided in `TUI.MainLoop`.

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

## Notes

In comparison to ncurses, this library does direct drawing to the
terminal. And so, there are no intermediate buffers. Moreover,
there is no optimization of screen updates.

Instead of optimizing updates, I use [Kitty's synchronous update
protocol]
(https://gitlab.com/gnachman/iterm2/-/wikis/synchronized-updates-spec).
This is protocol is ignored on unsupported terminals -- what you get
may or may not be usable. You have been warned.

# Known Limitations

## No Buffering

This is a design limitation.

I don't want to implement buffers and differential updates:
- It's not clear to me how that plays with sixel graphics
- I'm not sure exactly what ANSI-like features I want to support
- Ideally, such a heavy-weight solution would be opt-in.
- Seems like a source of nasty bugs.

What you lose without buffering is direct support for overlapping
windows. More tragically, there is no support for *clipping*, so it's
up to you not render out-of-bounds. This also makes makes scrolling
tricky to implement.

And so, the drawing model favors a left-to-right, top-to-bottom
drawing order, which is robust against the inability to clip painting
to a specific region of the screen.

Fortunately this dovetails with structural recursion. See `*split`,
and `*divide` in `TUI.Geometry` module. Or see `pack*` and `paint*` in
`TUI.View`. If you use these handy routines, you're on the garden
path.

The upside is that, since any `View` has complete freedom, you can do
things with layout that would be difficult in other frameworks.

## Future Work

### Keyboard Input

I plan to support this standard for [unambigous keyboard
input](https://sw.kovidgoyal.net/kitty/keyboard-protocol/).

For now, if you want to send a literal escape, you must press escape
twice.

### Feature Detection

For the moment, there is no support for termcap, terminfo, ANSI query
strings, etc. Therefore, there is no mechanism to support graceful
degredation and / or fallback functionality. Supported terminals are
supported, all others are not.

### Localization, Internationalization, and Accessiblity

I hope to get to this at some point. And I aim to do a decent job when
I get to it. But it's only worth it to me if other people end up using
this, and it's just too soon to say.
