# Kown Issues and Limitations

## No Buffering (and therefore, no scrolling)

This is a design limitation. See previous section.

What you lose without buffering is direct support for overlapping
windows. More tragically, there is no support for *clipping*, so it's
up to application code not render out-of-bounds. This also makes makes
scrolling components tricky to implement.

The drawing model favors a left-to-right, top-to-bottom drawing order,
which is robust against the inability to clip painting to a specific
region of the screen.

Fortunately, this dovetails with structural recursion. See `*split`,
and `distributeHorizonatal` in `TUI.Geometry` module. Or see `pack*`
and `paint*` in `TUI.View`.

The `VList` component aims to suport 

## Limited Key Handling

There's rudimentary ANSI decoding for basic keys. It's good enough to
at least start coding interactive things that run in your terminal,
but it's not yet enough to write a full-fledged application.

## Event Types are Hard Coded

The existing event types are hard-coded into the library mainloop and
input shim. The reason for this is that I can't figure out the best
way to generalize event handling without breaking other features of
the API that I think I like..

## <a name="kbd">Keyboard Input

I plan to support this standard for [unambigous keyboard
input](https://sw.kovidgoyal.net/kitty/keyboard-protocol/).

For now, if you want to send a literal escape, you must press escape
twice. This behavior will change as soon as I support the kitty
protocol, as I find it rather irritating.

Each breaking change will signal a point release.

When the full protocol is supported (including key report mode), I
will call it 1.0.

## Feature Detection

For the moment, there is no support for termcap, terminfo, ANSI query
strings, etc. Therefore, there is no mechanism to support graceful
degredation and / or fallback functionality. Supported terminals are
supported, all others are not.

## Localization, Internationalization, and Accessiblity

I hope to get to this at some point. And I aim to do a decent job when
I get to it. But it's only worth it to me if other people end up using
this, and it's just too soon to say.

## Compatibility Shim for Browsers

I can see no reason why this library couldn't eventually support
running in the browser, particularly with the help of an embedded
VTE. This could be an easy and fun way to create single page apps with
a certain nerdy aesthetic.

# Road Map

Here's a rough priority list, subject to change. The first release
will be `v0.1`. The next minor release will be `v0.2`, and will
include breaking changes.

- v0.1
  - continue developing ampii closely with idris-tui
  - set up CI github actions
  - write up tutorial.
  - pipeline to generate animated gifs.
- v0.2 (breaking API change)
  - define *all* keys and key-report mode
  - scrub API for bad names
	- favor `(.methods)` when appropriate (`window.splitLeft 1` over
      `splitLeft window 1`.
- v1.0
 - solve the buffering / scrolling problem
 - handle *all* the keys, and key-report mode.
 - implement asynchronous mainloop
