#
# Make Ernestine executable
#
# Author : Nicolas Lamirault
#

LISP=/usr/bin/sbcl
DIR=/home/nicolas/src/ernestine/

EXE=$1

function usage() {
    echo "usage: $0 executable-name"
    exit 1;
}


function make-ernestine() {

    echo "Make executable Ernestine : $EXE"
    $LISP \
        --eval "(push \"$DIR\" asdf:*central-registry*)" \
        --eval "(asdf:operate 'asdf:load-op :ernestine)" \
        --eval "(save-lisp-and-die \"$EXE\" :toplevel #'ernestine-gui:player :executable t))" \
        --eval "(quit)"
}

if [ -z "$EXE" ]
then
    usage
else
    make-ernestine
fi
