# Kown Issues and Limitations

## No Buffering

The trouble with buffering is that it forces us to re-implement much
of the same logic that the terminal itself already implements. This
library tries to simply pass through escape squences as directly as
possible.

Drawing order matters. Things drawn later occlude things which were
drawn earlier. I find this is fairly intuitive, actually. It also
dovetails well with structural recursion.

One downside is that it's up to the application to keep track of draw
attributes. This library tries to mitigate this as much as possible.

Another downside is that it's possible for unsanitized input to be
injected into your terminal, and that's kindof bad. There's no
mitigation for this as of yet.

## Incorrect String Sizing

A bunch of components and routines assume one codepoint = one
column. Addressing this is a big deal, and will happen later. For now,
there are various ways you can fudge sizing if you need to. The
passthrough-oriented design should be a bit more robust to issues
around this than if we relied on buffering.

## <a name="kbd">Keyboard Input

I plan to support this standard for [unambigous keyboard
input](https://sw.kovidgoyal.net/kitty/keyboard-protocol/).

For now, if you want to send a literal escape, you must press escape
twice. This behavior will change as soon as I support the kitty
protocol, as I find it rather irritating.

## No Async Events ##

Both `Base` and `InputShim` (and therefore `Default`) block reading on
stdin. This means it's not (yet) possible to have animated interfaces,
or games.

A MainLoop built on top of `idris2-linux` is certainly possible, and
would allow for a non-blocking event loop, which would be useful for a
variety of reasons.

## Feature Detection

For the moment, there is no support for termcap, terminfo, ANSI query
strings, etc. Therefore, there is no mechanism to support graceful
degredation and / or fallback functionality. Supported terminals are
supported, all others are not.

In particular, almost nothing in this libary works without support for
synchronized updates, so there's not really a "graceful" fallback in
this case.

## Localization, Internationalization, and Accessiblity

I hope to get to this at some point. And I aim to do a decent job when
I get to it. But it's only worth it to me if other people end up using
this, and it's just too soon to say.

## Compatibility Shim for Browsers

I can see no reason why this library couldn't eventually support
running in the browser, particularly with the help of an embedded
VTE. This could be an easy and fun way to create single page apps with
a certain nerdy aesthetic. It would also be neat way to host all the
demos on github.io.

# Road Map

Here's a rough priority list, subject to change. Development of v0.1
is still ongoing. Until v1.0 is reached, each minor version signals
breaking changes. After each release, bugfixes and non-breaking
changes may be backported to point releases.

- v0.1
  - continue developing ampii closely with idris-tui.
  - non-blocking mainloop (for games).
  - set up CI github actions.
  - pipeline to generate animated gifs from included examples.
- v0.2
  - full support for all keys (breaking changes to Key type).
  - scrub API for bad names
	- favor `(.methods)` when appropriate (`window.splitLeft 1` over
      `splitLeft window 1`.
  - write up tutorial.
- v0.3
  - feature detection / progressive enhancement.
  - support for gettext or something equivalent.
  - sanitize all strings sent to terminal (breaking change).
  - correctly calculate string column widths.
  - browser compatibility shim
	- demos hosted on github pages.
