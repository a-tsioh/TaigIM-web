TaigIM-web
==========

臺語輸入法，web-base + webservice

## Installation

Prepare an opam environment:

    # OSX with Homebrew
    brew install opam sqlite3 ; brew link -f sqlite3

Then install the `pgocaml` and `eliom` packages:

    opam repository add opamocsigen http://ocsigen.org/opam
    opam install pgocaml eliom ulex uunf ocp-indent merlin
