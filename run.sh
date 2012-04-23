#!/bin/sh

# Run as `./run.sh attach` to attach to working evk node

cd `dirname $0`

export ERL_LIBS=deps:apps

NODE_NAME=evk@localhost
COOKIE=evk

if [ "x"$1 == "x""attach" ]; then
    exec erl \
        -sname premsh@localhost \
        -remsh ${NODE_NAME} \
        -setcookie ${COOKIE}
else
    CONFIG=app.config
    make ${CONFIG}
    exec erl \
        +MBas gf +MRas gf +Mim true \
        -sname ${NODE_NAME} \
        -setcookie ${COOKIE} \
        -boot start_sasl \
        -config ${CONFIG} \
        +W w \
        +P 2000000 \
        -s evk
fi
