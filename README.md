# CoqRewriter

This project is licensed under the GNU General Public License 3.  See the file
LICENSE for more information.

There are two distinct ways to build.

To build with Coq, make sure you have Coq 8.5pl2 and Ocaml-4.04.0.

To compile standalone:

    cd plugin/src
    make

To run:

    cd plugin/src
    ./rewrite_test

(check out the file plugin/src/test1.ml for stand alone test cases)

To compile with Coq integration

    cd plugin
    make

To run some samples from the paper,

    cd plugin
    coqtop -q  -R "test-suite" ML_tutorial.Tests -R "src" ML_tutorial -I "src" < test-suite/example.v

