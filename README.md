AI for Discwar
==============

[Discwar][1] is a competitive sumo-wrestling style game in which the
purpose is to force the other player out of the ring. It supports
running an http server which will respond to post requests to control
AI agents in the ring.

This is my AI entry for that game.

  [1]: https://github.com/dkesler/Discwar

Compiling & Running
===================

The project is written in [Clojure][2] and uses the [Leiningen][3] 
build system.

Running should simply be

    lein run run.clj

If needed (e.g. for performance) it can also be compiled into
a standalone jar

    lein uberjar

once compiled running the jar should be as simple as

    java -jar discwar-ai-0.0.1-standalone.jar

  [2]: http://clojure.org/
  [3]: https://github.com/technomancy/leiningen
