# Plan9Hask

Experiments with Haskell and the 9P2000 protocol

- 9p2000 spec: <https://ericvh.github.io/9p-rfc/rfc9p2000.html>
- 9p2000.u spec: <https://ericvh.github.io/9p-rfc/rfc9p2000.u.html>

# Usage

To run the basic server run `stack run plan9haskserver-exe`.

To connect to the server you can use `9pfuse` from <https://9fans.github.io/plan9port/>. You will need to configure your system and user to enable FUSE.

`9pfuse -D -a "testattachname" 127.0.0.1:8564 /home/USER/mounttestdir`

The attach name is currently ignored so it can be anything.
