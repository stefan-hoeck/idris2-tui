# `idris-tui`

Aa framework for TUI applications in Idris.

List of supported terminals:
- iTerm2
- foot
- kitty
- alacritty

List of unsupported terminals:
- intelliJ (not tested)
- tmux (no sync update, to investigate)
- VTE terminals (not tested)
- anything not mentioned above.

For sixel image support, there's a soft dependency on
[chafa](https://hpjansson.org/chafa/)

# Overview

![screenshot](screenshot.png)

This library is written in pure Idris, and does not rely on ncurses or
any other existing TUI library.

It is still a work in progress, but you can start playing with it now.

Functionality is somewhat limited, but it is already useful for
writing custom editors for JSON files, and it is *almost* useful for
writing simple games. If you want to explore writing interactive
software in Idris, `idris-tui` runs right in your (modern) terminal.

# More Information

- [Tutorial](Tutorial.md)
- [Documentation](TBD)
- [Design Document](DESIGN.md)
- [Roadmap](ROADMAP.md)
