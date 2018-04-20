# CoqRewriter

This project is licensed under the GNU General Public License 3.  See the file
LICENSE for more information.

There are two distinct ways to build.

To build with Coq, make sure you have Coq 8.7.0 and Ocaml-4.04.0.  This build
was tested on a Mac running MacOS 10.12.6.

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
    make install

The examples directory contains code demonstrating use of the library.

Acknowledgements:

    The interface code in plugin.ml4 used the printAST code developed by Talia
    Ringer as its starting point (Available at https://github.com/uwplse/CoqAST)
    This code is licensed under the MIT license

    The files lib_coq.ml and lib_coq.mli come from Thomas Braibant's
    tutorial. (Available at https://github.com/braibant/coq-tutorial-ml-tactics)

    The files in Complex.v, Monad.v and Prelim.v in examples/complex come from
    the QWIRE project located at: https://github.com/jpaykin/QWIRE.

