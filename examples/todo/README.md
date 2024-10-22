# A TodoList Example

This example demonstrates some basic usage of the TUI framework.

It shows how to:

 - Read and write JSON from disk
 - Draw custom UI compnents
 - Respond to user input
 - Run the application mainloop.

## Usage

To build the package: `pack build`

To edit a file named todo.txt: `./run.sh todo.txt`

|---------|------------------------------------|
| Key     | Function                           |
|---------|------------------------------------|
| `+`     | Insert a new todo list item        |
| ` `     | Toggle completino of selected item |
| `q`     | Quit and save                      |
| `Enter` | Edit item description              |
| `Esc`   | Exit without saving                |
| `Up`    | Move up in the list                |
| `Down`  | Move down in the list              |
