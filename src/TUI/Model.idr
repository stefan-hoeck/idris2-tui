||| Minimalist terminal UI framework.
|||
||| Model is an interface which describes how to update a state value
||| for an arbitrary action.
|||
||| For TUI applications, input events mainly consist of keystrokes,
||| though we do not wish to limit ourselves to this notion.
module TUI.Model


%default total


||| A model knows how to update itself in response to an action.
public export
interface Model stateT actionT | stateT where
  ||| Update the model's state in response to an inner action.
  update : actionT -> stateT -> stateT
