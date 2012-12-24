#!/bin/bash

cd /home/zotonic/zotonic

git reset --hard
git pull
(cd ../zotonicdemo && git pull)
#make clean
make

