import Http

open Http.Data.Headers
open Http.Data
open Http

def onRequest (conn: Connection) : IO Unit := do
  conn.withHeaderStd .date        (← Time.DateTime.now .GMT)
  conn.withHeaderStd .contentType (Mime.mk (.standard .text) (String.CI.new "plain") (.empty |>.insert (String.CI.new "charset") "utf-8"))
  conn.withHeaderStd .transferEncoding #[TransferEncoding.chunked]

  conn.sendHeaders

  conn.write "some message"
  conn.write " here!"

  conn.end

def main : IO Unit :=
  Http.IO.Server.server
    (host := "127.0.0.1")
    (port := 8081)
    (onEnd := λconn _ => onRequest conn)
