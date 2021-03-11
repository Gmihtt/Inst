#!/bin/bash
workingDir=$(pwd)

cd src/JS/inst
pwd
screen -d -m npm run start

sleep 5s

cd $workingDir
stack run

