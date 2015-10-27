#!/bin/bash
cd /root/better-bot
stack build
stack test
stack install
mkdir dist
mkdir dist/lib
cp ~/.local/bin/better-bot-exe dist
sh cp_support_shared_libs.sh ~/.local/bin/better-bot-exe dist
find dist
