#/usr/bin/env bash

export HTTP_PROXY="http://eglbeprx001.esko-graphics.com:8080"
export HTTPS_PROXY=$HTTP_PROXY
export SOCKS_PROXY=$HTTP_PROXY

export NO_PROXY="localhost,127.0.0.1,$USERDNSDOMAIN"

$1 $2 $3 $4 $5 $6

unset HTTP_PROXY
unset HTTPS_PROXY
unset SOCKS_PROXY
unset NO_PROXY
