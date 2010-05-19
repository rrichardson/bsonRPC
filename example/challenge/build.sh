#!/usr/bin/env bash

ghc --make -threaded -o client client.hs util.hs 
ghc --make -threaded -o server server.hs util.hs
