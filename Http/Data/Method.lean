-- | A method is a verb that describes the action to be performed.
inductive Method where
  | get
  | head
  | post
  | put
  | delete
  | connect
  | options
  | trace
  | patch

instance : ToString Method where
  toString
    | Method.get     => "GET"
    | Method.head    => "HEAD"
    | Method.post    => "POST"
    | Method.put     => "PUT"
    | Method.delete  => "DELETE"
    | Method.connect => "CONNECT"
    | Method.options => "OPTIONS"
    | Method.trace   => "TRACE"
    | Method.patch   => "PATCH"

def Method.fromString : String → Option Method
    | "GET"     => some Method.get     
    | "HEAD"    => some Method.head    
    | "POST"    => some Method.post    
    | "PUT"     => some Method.put     
    | "DELETE"  => some Method.delete  
    | "CONNECT" => some Method.connect 
    | "OPTIONS" => some Method.options 
    | "TRACE"   => some Method.trace   
    | "PATCH"   => some Method.patch   
    | _         => none
  
