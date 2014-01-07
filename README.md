TaigIM-web
==========

臺語輸入法，web-base + webservice

## Installation

Prepare an opam environment:

    # OSX with Homebrew
    brew install opam sqlite3 ; brew link -f sqlite3

Then install the `pgocaml` and `eliom` packages:

    opam repository add opamocsigen http://ocsigen.org/opam
    opam install dbm pgocaml eliom ulex uunf ocp-indent merlin

...and import the PostgreSQL database (OSX binary is available on <http://postgresapp.com/>):

    bzcat pgsqlDB.dump.bz2 | psql

Finally, start the server with:

    make test.byte

When you see `ocsigenserver` on screen, connect to <http://localhost:8080/lookup/%E3%84%92%E3%84%A7> to confirm that the installation has worked.
