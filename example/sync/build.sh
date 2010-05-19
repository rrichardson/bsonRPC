#!/usr/bin/env bash

ghc --make -threaded -XBangPatterns -o client client.hs 
ghc --make -threaded -o server server.hs
