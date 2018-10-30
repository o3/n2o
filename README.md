# N2O for Haskell

[![Build Status](https://travis-ci.org/xafizoff/n2o-hs.svg?branch=master)](https://travis-ci.org/xafizoff/n2o-hs)

Features
--------

* Endpoints: poor man's WebSocket and static HTTP server
* Formatters: BERT
* Protocols: <a href="https://haskell.n2o.space/man/protocols.htm">CLIENT</a>, <a href="https://haskell.n2o.space/man/protocols.htm">NITRO</a>
* High Performance Protocol Relay
* Smallest possible codebase — 500 LOC

N2O defines a way we scale protocols, database schema, applications and
services across companies, formatters, views and presentation layers.
At the core N2O folds a list of protocols and their handlers providing
a minimal type-level specification for general purpose application protocol tract.

As example this Haskell version of N2O is shipped with Nitro protocol
implementation, that listens the tract and push prerendered JavaScript
events back to the channel. This smart and simple reactive way
of client-server interaction first was used by Rusty Klophaus in
his Nitrogen web framework, that was pushed forward since then in
N2O by Andy Melnikov and Marat Khafizov.

https://haskell.n2o.space

Setup
-----

```sh
stack build
stack exec n2o-sample
open http://localhost:3000/samples/static/index.html
```

Nitro Protocol Demo
-------------------

### Extensions and imports

```haskell
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main (main) where

import Network.N2O
import Network.N2O.Web
import Network.N2O.Protocols hiding (Init)
import Network.N2O.Nitro
import Prelude hiding (id)
```

### Static Server and Page Router

```haskell
  data Example = Greet deriving (Show, Eq, Read)

  main = runServer "localhost" 3000 cx
  cx = createCx router
  router cx@Cx{cxReq=Req{reqPath=path}} =
      let handler = case path of
          "/ws/samples/static/index.html" -> index
          "/ws/samples/static/about.html" -> about
                                        _ -> index
      in cx{cxHandler=handler}
```

### Nitro Page Sample

```haskell
  index Init = do
      updateText "system" "What is your name?"
      wireEl button{id="send", postback=Just Greet, source=["name"]}

  index (Message Greet) = do
      Just name <- get "name" -- wf:q/1
      updateText "system" ("Hello, " <> jsEscape name <> "!")

  about Init =
      updateText "app" "This is the N2O Hello World App"
```

Credits
-------

* Andy Melnikov
* Marat Khafizov
* Maxim Sokhatsky

