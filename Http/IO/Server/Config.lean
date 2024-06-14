import Http.Data.Uri
import Http.Config

namespace Http.IO.Server
open Http.Data

/-- Structure to configure a HTTP web server -/
structure Config where
  /-- Server name in the response headers -/
  name: Option String

  /-- Host checking thing. It checks if the host header matches this. -/
  host : Option String

  /-- Option to add Connection: Closed when it's shutting down -/
  closeOnShutdown: Bool

  /-- If it will use keep alive headers to multiple requests at the same connection-/
  tcpKeepAlive: Bool

  /-- Time in miliseconds to close the connection after some request is sent -/
  idleTimeout : Nat

  /-- Maximum of keep alive connections -/
  maxKeepAliveRequests : Option Nat

  /-- Configurations for parsing a message. -/
  messageConfig : MessageConfig

instance : Inhabited Config where
  default :=
    { name := some "Http.lean"
    , host := none
    , closeOnShutdown := true
    , tcpKeepAlive := true
    , idleTimeout := 1000
    , maxKeepAliveRequests := some 1000
    , messageConfig := Inhabited.default
    }
