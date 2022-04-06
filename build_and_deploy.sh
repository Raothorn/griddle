#!/bin/bash

cd ~/source/repos/griddle/
elm make src/Main.elm --output=../raothorn.github.io/main.js

cd ~/source/repos/raothorn.github.io
git add .
git commit -m "Update"
git push origin main
