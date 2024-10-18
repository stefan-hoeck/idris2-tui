module TUI.Component.PushButton


import TUI.View
import TUI.Component


%default total


||| A button that will bring up a modal component when it receives `Enter`
export
record PushButton valueT where
  constructor MkPushButton
  label : String
  component : Component valueT
  value : Maybe valueT

render : PushButton _ -> String
render self = "[\{self.label}]"

export
View (PushButton _) where
  size self = size $ render self
  paint state window self = withState state $ do
    showTextAt window.nw $ render self

export
handle : Component.Handler (PushButton valueT) valueT
handle Enter self = push self.component onMerge
  where
    onMerge : Maybe valueT -> PushButton valueT
    onMerge value = {value := value} self
handle _     self = ignore

export
pushButton : String -> Component valueT -> Component valueT
pushButton label modal = component {
  state   = (MkPushButton label modal Nothing),
  handler = handle,
  get     = (.value)
}
