#!/bin/sh

stack build
echo ":set -i$(stack path --dist-dir)/build/hserv/autogen/" > .ghci
