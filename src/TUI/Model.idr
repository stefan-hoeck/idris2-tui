||| Minimalist terminal UI framework.
|||
||| Model is an interface which describes how to update a state value
||| for an arbitrary action.
|||
||| For TUI applications, input events mainly consist of keystrokes,
||| though we do not wish to limit ourselves to this notion.
module TUI.Model


||| A model knows how to update itself in response to an action.
public export
interface Model stateT valueT actionT where
  update : actionT -> stateT -> Either stateT valueT
