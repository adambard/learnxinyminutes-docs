---
category: tool
tool: messagepack
filename: learnmessagepack.mpac
contributors:
  - ["Gabriel Chuan", "https://github.com/gczh"]
translators:
    - ["Alessio Benvenuti", "https://github.com/alessiobenvenuti"]
lang: it-it
---

MessagePack è un efficiente formato di serializzazione binaria. Ti consente di scambiare dati tra più linguaggi come JSON. I vantaggi rispetto ad altri formati è che è più veloce e più piccolo.

In MessagePack, i numeri interi piccoli sono codificati in un singolo byte e le tipiche stringhe brevi richiedono solo un byte aggiuntivo oltre alle stringhe stesse. Ciò rende MessagePack utile per una trasmissione efficiente via cavo.

```

# 0. Comprensione della struttura ====

JSON, 40 Bytes UTF-8
	
----------------------------------------------
| {"name":"John Doe","age":12}		         |
----------------------------------------------
|  {"         | 7B 22                        |
|    name     | 6E 61 6D 65                  |
|  ":"        | 22 3A 22                     |
|    John Doe | 4A 6F 68 6E 20 44 6F 65      |
|  ","        | 22 2C 22                     |
|    age      | 61 67 65                     |
|  ":         | 22 3A 20                     |
|    12       | 31 32                        |
|  }          | 7D                           |
----------------------------------------------


MessagePack, 27 Bytes UTF-8
	
----------------------------------------------
| ‚¤name¨John Doe£age.12                     |
----------------------------------------------
|  ‚¤         | 82 84                        |
|    name     | 6E 61 6D 65                  |
|  ¨          | A8                           |
|    John Doe | 4A 6F 68 6E 20 44 6F 65      |
|  £          | A3                           |
|    age      | 61 67 65                     |
|  .          | 0C                           |
|    12       | 31 32                        |
----------------------------------------------

# 1. JAVA ====

""" Installazione con Maven
"""

<dependencies>
  ...
  <dependency>
    <groupId>org.msgpack</groupId>
    <artifactId>msgpack</artifactId>
    <version>${msgpack.version}</version>
  </dependency>
  ...
</dependencies>


""" Semplice Serializzazione/deserializzazione
"""

// Crea oggetti serializzati.
List<String> src = new ArrayList<String>();
src.add("msgpack");
src.add("kumofs");

MessagePack msgpack = new MessagePack();
// Serializza
byte[] raw = msgpack.write(src);

// Deserializza direttamente usando un modello
List<String> dst1 = msgpack.read(raw, Templates.tList(Templates.TString));
System.out.println(dst1.get(0));
System.out.println(dst1.get(1));

// Oppure, Deserializza in un valore, quindi converti il tipo.
Value dynamic = msgpack.read(raw);
List<String> dst2 = new Converter(dynamic)
    .read(Templates.tList(Templates.TString));
System.out.println(dst2.get(0));
System.out.println(dst2.get(1));


# 2. RUBY ====

""" Installazione della gemma
"""

gem install msgpack

"""  API di Streaming
"""

# serializza a 2-elementi di un array [e1, e2]
pk = MessagePack::Packer.new(io)
pk.write_array_header(2).write(e1).write(e2).flush

# deserializzare oggetti da un IO
u = MessagePack::Unpacker.new(io)
u.each { |obj| ... }

# deserializzazione guidata dagli eventi
def on_read(data)
  @u ||= MessagePack::Unpacker.new
  @u.feed_each(data) { |obj| ... }
end

# 3. NODE.JS ====

""" Installazione con NPM
"""

npm install msgpack5 --save

""" Utilizzando Node
"""

var msgpack = require('msgpack5')() // namespace delle nostre estensioni
  , a       = new MyType(2, 'a')
  , encode  = msgpack.encode
  , decode  = msgpack.decode

msgpack.register(0x42, MyType, mytipeEncode, mytipeDecode)

console.log(encode({ 'hello': 'world' }).toString('hex'))
// 81a568656c6c6fa5776f726c64
console.log(decode(encode({ 'hello': 'world' })))
// { hello: 'world' }
console.log(encode(a).toString('hex'))
// d5426161
console.log(decode(encode(a)) instanceof MyType)
// true
console.log(decode(encode(a)))
// { value: 'a', size: 2 }

function MyType(size, value) {
  this.value = value
  this.size  = size
}

function mytipeEncode(obj) {
  var buf = new Buffer(obj.size)
  buf.fill(obj.value)
  return buf
}

function mytipeDecode(data) {
  var result = new MyType(data.length, data.toString('utf8', 0, 1))
    , i

  for (i = 0; i < data.length; i++) {
    if (data.readUInt8(0) != data.readUInt8(i)) {
      throw new Error('dovrebbe essere tutto identico')
    }
  }

  return result
}

```


# Fonti

- [MessagePack](http://msgpack.org/index.html)
- [MsgPack vs. JSON: Riduci il traffico client-server del 50% con una riga di codice](http://indiegamr.com/cut-your-data-exchange-traffic-by-up-to-50-with-one-line-of-code-msgpack-vs-json/)
