# Quad CI

A reimplementation of quad-ci in Haskell following Marco Sampellegrini's [Simple Haskell Handbook](https://marcosampellegrini.com/simple-haskell-book).

### Codebase overview (stolen from [Marco's quad-ci repo](https://github.com/alpacaaa/quad-ci))

_`src/Core.hs`_  
Domain types (`Build`, `Pipeline` etc.) along with main state machine (`progress`)

_`src/Docker.hs`_  
Talks to Docker api

_`src/Runner.hs`_  
Runs a single build, collecting logs (`Core.collectLogs`) and processing state updates (`Core.progress`)

_`src/JobHandler.hs`_  
Introduces `Job` type, which is just a `Build` that can be _queued_ and _scheduled_

_`src/JobHandler/Memory.hs`_  
An in-memory implementation of `JobHandler`, built on top of STM

_`src/Github.hs`_  
Talks to Github api

_`src/Agent.hs`_  
Agents ask the server for work to do, run builds (`Runner`) and send updates back to the server

_`src/Server.hs`_  
The server collects jobs to be run (when receiving webhook events). It keeps an internal job queue (`JobHandler`) exposed as an http api (used by web ui)

_`src/Cli.hs`_  
Main entrypoint. Calls either `Server.run` or `Agent.run`

_`src/Socket.hs`_  
Low-level code to send http requests to a socket. Not interesting, can be ignored.

---

For a full overview of the codebase, check out the [Simple Haskell Handbook](https://marcosampellegrini.com/simple-haskell-book) where we start from **zero lines of code** and build Quad CI _from scratch_!

