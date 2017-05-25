#!/usr/bin/env bash

python -m SimpleHTTPServer 4242 &
echo $! > python.pid

function kill_python() {
    kill $(cat python.pid)
    rm -f python.pid
}
trap kill_python EXIT

make

sbcl --noinform --noprint --no-sysinit --no-userinit --disable-debugger \
     --load "$PWD"/.quicklocal/setup.lisp \
     --eval "(push \""$PWD"/\" asdf:*central-registry*)" \
     --eval "(ql:quickload :laap-test)" \
     --eval "(unless (laap/test:run-all-tests) (uiop:quit 1))" \
     --quit
