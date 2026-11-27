# Yet another Haskell 9p2000 file server protocol implementation

There are a few Haskell implementations of the 9p2000 file server protocol <https://ericvh.github.io/9p-rfc/rfc9p2000.html>.
It's designed to be relatively easy to implement and I wanted to learn more about parsing protocols in Haskell.
Going through the implementation has been a good learning experience so I would recommend it to anyone.

The protocol lets you serve a file system similar to NFS or SMB but was also designed to serve synthetic file systems for the
plan 9 operating system.
In plan 9 a synthetic file system is used serve APIs for accessing devices like the keyboard, mouse and graphics system.

- <https://hackage.haskell.org/package/NineP> provides a parser for the protocol 
- <https://hackage.haskell.org/package/Network-NineP> provides a monadic interface for serving a synthetic file system
  - <https://github.com/l29ah/Network-NineP> 
- <https://www.hobson.space/posts/9p/> provides a `freer-simple` based interface for serving a synthetic file system
  - <https://github.com/yobson/ninepmonad>

What could this be used for with Haskell? 
It could be used to interface with backup software like `restic` or `borg`.
These programs allow you to mount backup snapshots through FUSE.
A similar implementation could be used to serve `git-annex` stores over a local network.
You would probably want to use something like `stunnel` or an `ssh` tunnel in that case to secure the connection since `9pfuse` 
does not support an encrypted connection.

It can also be used as a generic way to manage multiple streams of data over a single connection.
If you have several different types that you want to send across a connection you can serve each type over a different synthetic file.
Using a jsonlines format each synthetic file could be configured with a parser that returns the appropriate type.
Binary data can also be exchanged over a separate synthetic file.

The test server can be started with:

`stack run plan9haskserver-exe`

And the file system can be mounted through <https://9fans.github.io/plan9port/> `9pfuse` :

`9pfuse -D -a "testattachname" 127.0.0.1:8564 /home/USER/mounttestdir`

# Parser implementation motivation/strategy

The existing Haskell 9p2000 projects are designed to make it easy to serve a synthetic file system.
This repository is setup to allow me to experiment with protocols and protocol handling.

A record-of-functions interface is used to separate the protocol from the handling.
This would let me try to see if parsing messages into records has any overhead.
I can try different parser implementations and effect systems and compare them to see what the differences are.
To start with I used `network` and `attoparsec` directly but I also want to experiment with `conduit` and `bluefin` streaming.

Even though doing things at a lower level introduces more corner cases it was an enjoyable experience.
`network` and `attoparsec` are truly beautiful libraries.

I'm hoping that the interface will be lightweight enough.
It will probably prevent some inlining optimizations but as long as the allocation overhead is low enough that is ok for me.

```haskell
data RequestHandler a = RequestHandler
  { handleTVersion :: Msg.Tag -> Data.Word.Word32 -> Data.Text.Text -> a,
    handleTAuth :: Msg.Tag -> Msg.AuthFid -> Msg.Username -> Msg.AttachName -> a,
    handleTFlush :: Msg.Tag -> Msg.Tag -> a,
    handleTAttach :: Msg.Tag -> Msg.Fid -> Msg.AuthFid -> Msg.Username -> Msg.AttachName -> a,
    handleTWalk :: Msg.Tag -> Msg.Fid -> Msg.Fid -> Msg.ArrayElementCount -> [Data.Text.Text] -> a,
    handleTOpen :: Msg.Tag -> Msg.Fid -> Msg.OpenMode -> a,
    handleTCreate :: Msg.Tag -> Msg.Fid -> Data.Text.Text -> Msg.Permissions -> Msg.OpenMode -> a,
    handleTRead :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Msg.ByteCount -> a,
    handleTWrite :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Msg.ByteCount -> Data.ByteString.ByteString -> a,
    handleTClunk :: Msg.Tag -> Msg.Fid -> a,
    handleTRemove :: Msg.Tag -> Msg.Fid -> a,
    handleTStat :: Msg.Tag -> Msg.Fid -> a,
    handleTWstat :: Msg.Tag -> Msg.Fid -> Msg.VarArgByteCount -> Msg.Stat -> a,
    handleParseError :: Parser.ParseError -> a
  }
```

The 9p2000 protocol uses a style where every message and variably sized piece of data starts with a size header.
I use `attoparsec`'s incremental parsing to separate messages into individual `ByteString`s.
The maximum message size is negotiated between the client and the server.
For writes and reads I will probably be testing messages in the 4kb to 8kb range.

I could integrate the message parsing into this parser but this setup lets me try different binary parsers like `cereal` and `binary`.
It could turn out that this setup introduces its own inefficiencies but for now it's convenient.

```haskell
msgParser :: Data.Word.Word32 -> Data.Attoparsec.ByteString.Parser (Int, Data.ByteString.ByteString)
msgParser maxSize = do
  sizeBytes <- Data.Attoparsec.ByteString.take msgSizeBytes
  let parsedSize = Data.Serialize.Get.runGet Data.Serialize.Get.getWord32le sizeBytes
  size <- case parsedSize of
    (Right sizeWord) | sizeWord < 4 + 1 + 2 -> fail "size cannot fit the message type and tag"
    (Right sizeWord) | sizeWord > maxSize -> fail "size is larger than the maximum"
    (Right sizeWord) -> pure $ (fromIntegral sizeWord :: Int)
    (Left err) -> fail err
  msgBytes <- Data.Attoparsec.ByteString.take (size - msgSizeBytes)
  return (size, msgBytes)
```

A typical 9p2000 message has a few fixed size unsigned integers and sometimes some size-prefixed binary data like strings.
For the first cut at message parsing I used some manual bit-bashing code.
I plan to try out `cereal` but also Liquid Haskell and the "Scrap your bounds checks" approach from
<https://github.com/Gabriella439/slides/blob/main/liquidhaskell/slides.md>.

Even the code is kind of clumsy using Haskell naturally made it look better which was a fun surprise.

```haskell
tVersionCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tVersionCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise = case Parser.parsePlan9Str $ Data.ByteString.drop (Msg.messageTypeAndTagSize + 4) msgBytes of
      Right versionStr -> callbacks.handleTVersion tag (Parser.convertLeWord32 msgBytes [3, 4, 5, 6]) versionStr
      Left err -> callbacks.handleParseError err
```

# Protocol handler implementation

Despite being a relatively simple protocol to parse handling `9p2000` messages it is a bit tricky to handle the protocol messages properly.
[MinimalHandler.hs](../../plan9haskserver/src/Network/Plan9Hask/MinimalHandler.hs) implements a empty read-only filesystem but it still takes up a decent bit of code and it's still not complete.
The client chooses its own `Word16` `tag` to identify a request which it then uses to identify the corresponding response.
The client is responsible for making sure that a tag is currently unused when it sends it.
The tag space is isolated to the connection so it isn't that bad but there's a `Tflush` call that cancels an active request.
In a multi-threaded implementation you would want to ensure that a `Tflush` in a queue somewhere doesn't cancel a request that happened after the client reuses a tag.

`fid`s which are `Word32`s are the equivalent of a file descriptor/file handle.
These are also chosen arbitrarily by the client so the server has to track their state and which ones are in use.

[MapFsHandler.hs](../../plan9haskserver/src/Network/Plan9Hask/MapFsHandler.hs) implements a read-only file system stored in `Data.Map`.
I started by copying `MinimalHandler` and refactoring it.
This ended up being a good way to practice refactoring Haskell since it multiplied the number of error conditions.

I also want to make a read-write version of `MapFsHandler` which may or may not be a separate module.

It was pretty cool to see `9pfuse` connect to the server and be able to navigate the file system using the shell.
On my Linux system mounting the system starts up a bunch of desktop probes for things like icons and trash folders.
It's pretty easy to generate some non-trivial test data just by entering a few regular shell commands.

Since there is a message separation phase before parsing the individual message `ByteString`s could be saved in a test file and replayed to benchmark parsing and handling speed.

There's still a lot of work to do and tricky cases like `..` paths to handle but this has been a good learning experience.
Hopefully this report helps someone else who is curious about protocol parsing and file server protocols in Haskell.

# Resources

- <https://principia-softwarica.org/> a course and books covering the implementation of plan 9
- <https://www.youtube.com/@adventuresin9> covers many aspects of the plan 9 os
- Diod 9p file server <https://github.com/chaos/diod>
  - protocol specs: <https://github.com/chaos/diod/blob/master/protocol.md>
- 9p2000 spec: <https://ericvh.github.io/9p-rfc/rfc9p2000.html>
- 9p2000.u spec: <https://ericvh.github.io/9p-rfc/rfc9p2000.u.html>
