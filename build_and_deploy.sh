#!/bin/bash

cd ~/source/repos/griddle/
elm make src/Main.elm --output=../raothorn.github.io/main.js
cp -r resources/ ../raothorn.github.io/

cd ~/source/repos/raothorn.github.io
git add .
git commit -m "Update"
git push origin main
