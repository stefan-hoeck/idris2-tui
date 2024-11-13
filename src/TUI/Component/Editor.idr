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

||| A Component for editing values.
|||
||| XXX: It's unclear if this component still serves a purpose or if
||| the `Modal` mechanism makes it obsolete. However, it's used by
||| `Form` at the moment, so it stays.
module TUI.Component.Editor


import Data.Maybe
import Data.String
import TUI.Component
import TUI.Util
import TUI.Zipper.List


%default total


||| Defines how to create an editor for a value.
|||
||| XXX: this interface may be obsolete with the new component
||| arch.
public export
interface Editable (0 events : List Type) valueT | valueT
where
  fromValue  : valueT -> Component (HSum events) valueT
  blank      : Component (HSum events) valueT

||| Construct a component for an editable type.
|||
||| The value may or may not be known.
export
editable
  :  {0 events : List Type}
  -> Editable events valueT
  => Maybe valueT
  -> Component (HSum events) valueT
editable Nothing  = blank
editable (Just v) = fromValue v
