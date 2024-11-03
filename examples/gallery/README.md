# TUI Component Gallery

This is a work in progress, with the goal of demonstrating most
features of the library.

To run this demo:

```
pack install-deps
pack build
./run.sh
```

## Changing the Modal View

With no arguments given, the modals in the demo will nest visually.

| Arguments           | Behavior                   |
|---------------------|----------------------------|
| `./run.sh inset`    | Same as default            |
| `./run.sh fromTop`  | Modals stack top-down      |
| `./run.sh fromLeft` | Modals stack left-to-right |
