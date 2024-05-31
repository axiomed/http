namespace Http.IO

/-- Buffer represented as an array of byte arrays -/
abbrev Buffer := Array ByteArray

/-- Class to define how a type can be converted into a buffer -/
class ToBuffer (α: Type) where
  toBuffer : Buffer → α → Buffer

def Buffer.empty : Buffer := #[]

/-- Function to push data into a buffer using ToBuffer -/
def Buffer.push [ToBuffer α] (buffer: Buffer) (data: α) : Buffer :=
  Array.append buffer (ToBuffer.toBuffer #[] data)

def Buffer.pushRaw (buffer: Buffer) (data: ByteArray) : Buffer :=
  Array.push buffer data

/-- BufferBuilder is a state transformer over IO with Buffer state -/
abbrev BufferBuilder (α: Type) := StateT Buffer Id α

/-- Write data to the buffer using BufferBuilder -/
def BufferBuilder.write [ToBuffer α] (data: α) : BufferBuilder Unit :=
  StateT.modifyGet (λp => ((), p.push data))

/-- Run the BufferBuilder to produce the final Buffer -/
def BufferBuilder.run (buffer: Buffer) (x: BufferBuilder Unit) : Buffer :=
  Id.run $ Prod.snd <$> StateT.run x buffer

/-- Class to define how a type can be transformed into BufferBuilder actions -/
class Serialize (α: Type) where
  serialize : α → BufferBuilder Unit

-- Instances

instance [ToBuffer α] : Serialize α where
  serialize x := StateT.modifyGet (λp => ((), ToBuffer.toBuffer p x))

instance [Serialize α] : ToBuffer α where
  toBuffer buffer data :=
    let ata := Serialize.serialize data
    BufferBuilder.run buffer ata

instance : ToBuffer String where
  toBuffer buffer data := buffer.pushRaw data.toUTF8

instance : ToBuffer ByteArray where
  toBuffer buffer data := buffer.pushRaw data
