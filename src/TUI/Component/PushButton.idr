-- BSD 3-Clause License
--
-- Copyright (c) 2023, Brandon Lewis
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module TUI.Component.PushButton


import TUI.View
import TUI.Component


%default total


||| A button that will bring up a modal component when it receives `Enter`
export
record PushButton {0 events : List Type} valueT where
  constructor MkPushButton
  label : String
  component : Component (HSum events) valueT
  value : Maybe valueT

render : PushButton _ -> String
render self = "[\{self.label}]"

export
View (PushButton _) where
  size self = size $ render self
  paint state window self = withState state $ do
    showTextAt window.nw $ render self

export
onKey
  : {0 events : List Type}
  -> Single.Handler {events} (PushButton {events} valueT) valueT Key
onKey Enter self = push self.component onMerge
  where
    onMerge : Maybe valueT -> PushButton {events} valueT
    onMerge value = {value := value} self
onKey _     self = ignore

export
pushButton
  :  {0 events : List Type}
  -> Has Key events
  => String
  -> Component (HSum events) valueT
  -> Component (HSum events) valueT
pushButton label modal = component {
  state   = MkPushButton label modal Nothing,
  handler = only onKey,
  get     = (.value)
}
