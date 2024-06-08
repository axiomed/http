import Lean.Data.Hashmap
import Http.Router.Pattern
import Http.Util.KeyedArray

namespace Http.Router
open Http.Util
open Lean

abbrev Capture := Option String

deriving instance Repr for AssocList

inductive Tree (α: Type) where
  | node (branches: AssocList (String) (Tree α)) (otherwise: Option (Tree α))
  | goto (caps: List (String × Nat)) (alt: (KeyedArray (caps.map Prod.fst) String) → α)
  deriving Inhabited

def Tree.empty : Tree α := Tree.node AssocList.empty none

def Tree.modify (tree: Tree α) (k: String) (f: Tree α → Tree α) : Tree α :=
  match tree with
  | .node branches otherwise => .node (branches.insert k (f ((branches.find? k).getD Tree.empty))) otherwise
  | .goto a n => .node (AssocList.empty.insert k (.goto a n)) (some (.goto a n))

def Tree.modifyGoto (tree: Tree α) (f: (Tree α) → Tree α) (default: Thunk (Tree α)) : Tree α :=
  match tree with
  | .node branches otherwise => .node branches (f (Option.getD otherwise default.get))
  | .goto a n => .node Inhabited.default (some (f (.goto a n)))

def Tree.insert (tree: Tree α) (pat: Array Pattern) (value: (KeyedArray ((Pattern.captures pat).map Prod.fst) String) → α) : Tree α :=
  let goto := Thunk.mk λ_ => (.goto (Pattern.captures pat) value)

  let emp := Thunk.mk λ_ => Tree.empty

  let rec acc (tree: Tree α) (idx: Nat) : Tree α :=
    if h : idx < pat.size then
      match pat[idx] with
      | .name n => tree.modify n (acc · (idx + 1))
      | .capture _ => tree.modifyGoto (acc · (idx + 1)) emp
      | .wildcard  => tree.modifyGoto (acc · (idx + 1)) emp
    else
      tree.modifyGoto id goto
  acc tree 0

def Tree.find? (tree: Tree α) (path: Array String) : Option α :=
  let rec acc (tree: Tree α) (idx: Nat) : Option α :=
    if h : idx < path.size then
      let cur := path.get ⟨idx, h⟩
      match tree with
      | .node x o =>
        if let some branch := x.find? cur then
          acc branch (idx + 1)
        else
          match o with
          | some res => acc res (idx + 1)
          | none     => none
      | .goto caps res =>
        if _ : idx + 1 = path.size then
          let maps := caps.map (path.get! ∘ Prod.snd)
          let caps₂ := KeyedArray.mk (maps.toArray) $ by simp [Array.get!, maps]
          some (res caps₂)
        else
          none
    else
      none
    termination_by (path.size - idx)
  acc tree 0
