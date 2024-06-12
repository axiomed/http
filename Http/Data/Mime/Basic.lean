import CaseInsensitive
import Lean.Data.HashMap
import Http.Classes

namespace Http.Data
open Http.Classes
open Lean

deriving instance Repr for AssocList

inductive GeneralType.Standard
  | text
  | image
  | audio
  | video
  | application
  | message
  | multipart
  deriving Repr

instance : Parseable GeneralType.Standard where
  parse str := match str.toLower with
  | "text" => some .text
  | "image" => some .image
  | "audio" => some .audio
  | "video" => some .video
  | "application" => some .application
  | "message" => some .message
  | "multipart" => some .multipart
  | _ => none

instance : Canonical .text GeneralType.Standard where
  repr
  | .text => "text"
  | .image => "image"
  | .audio => "audio"
  | .video => "video"
  | .application => "application"
  | .message => "message"
  | .multipart => "multipart"


/-- There are two classes of types. Discrete types represent a single file or medium, Multipart
correpsond to documents that contains multiple parts with multiple MIME type or multiple files in
a single transaction.

* Reference: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types
-/
inductive GeneralType.Standard.Class
  | discrete
  | multipart

/-- Gets the type of a media type. -/
def GeneralType.Standard.defineType : GeneralType.Standard → GeneralType.Standard.Class
  | .text | .image | .audio | .video | .application => .discrete
  | .message | .multipart => .multipart

/-- The type of a MIME type. -/
inductive GeneralType
  | standard (val: GeneralType.Standard)
  | custom (val: String.CI)
  deriving Repr

instance : Canonical .text GeneralType where
  repr
    | .standard val => Canonical.text val
    | .custom val => Canonical.text val

instance : Standard GeneralType GeneralType.Standard where
  custom := GeneralType.custom ∘ String.CI.new
  standard := GeneralType.standard

/-- Definition of a IANA Media Type.

* Reference: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types
-/
structure Mime where
  /-- The general category. -/
  type: GeneralType

  /-- The exact kind of data. -/
  subType: String.CI

  /-- Mime type parameters. -/
  params: AssocList String.CI String
  deriving Repr

instance : Canonical .text Mime where
  repr mime :=
    let paramsStr := mime.params.foldl (fun acc k v => acc ++ "; " ++ toString k ++ "=" ++ v) ""
    Canonical.text mime.type ++ "/" ++ toString mime.subType ++ paramsStr

/-- Create a Mime type with no parameters. -/
def Mime.new (type subType : String) : Mime :=
  { type := Standard.parse type, subType := String.CI.new subType, params := AssocList.empty }

inductive Any (α: Type) where
  | any
  | specific (val: α)
  deriving Repr

instance [Canonical .text α] : Canonical .text (Any α) where
  repr
    | .any => "*"
    | .specific r => Canonical.text r

/-- The range of accepted MIME.

* Reference: https://httpwg.org/specs/rfc9110.html#field.accept
-/
structure MediaRange where
  /-- The general category or * that represents any. -/
  type: Any GeneralType

  /-- The exact kind of data or * that represents any. -/
  subType: Any String.CI

  /-- Mime type parameters. -/
  params: AssocList String.CI String
  deriving Repr

instance : Canonical .text MediaRange where
  repr mr :=
    let paramsStr := mr.params.foldl (fun acc k v => acc ++ ", " ++ toString k ++ "=" ++ v) ""
    Canonical.text mr.type ++ "/" ++ Canonical.text mr.subType ++ paramsStr
