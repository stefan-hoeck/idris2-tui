# TUI Component Gallery

This is a work in progress, with the goal of demonstrating most
features of the library.

To run this demo:

```
pack install-deps
pack build
./run.sh
```
## Changing the Modal Stacking Behavior

By default, modals nest visually. This is to make it as obvious as
possible what is happening when you call `push`.

But we can can draw the modal stack in different ways, you can explore
these by passing different arguments to the demo.

With no arguments given, the modals in the demo will nest visually.

| Arguments           | Behavior                        |
|---------------------|---------------------------------|
| `./run.sh topmost`  | Only render top-most component. |
| `./run.sh inset`    | Same as default.                |
| `./run.sh fromTop`  | Stack to-to-bottom.             |
| `./run.sh fromLeft` | Stack left-to-right.            |
| `./run.sh centered` | Center in frame.                |
