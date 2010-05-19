#!/usr/bin/env bash

ghc --make -O3 -threaded -XBangPatterns -o client client.hs util.hs
ghc --make -O3 -threaded -o server server.hs
#ghc --make -O3 -XBangPatterns -o client client.hs util.hs
#ghc --make -O3 -o server server.hs
