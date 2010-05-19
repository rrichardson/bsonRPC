#!/usr/bin/env bash

ghc --make -threaded -o client client.hs 
ghc --make -threaded -o server server.hs
