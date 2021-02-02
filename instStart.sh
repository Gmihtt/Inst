#!/bin/bash
workingDir=$(pwd)

cd src/APP/Scripts/JS/inst
screen -d -m npm run login
#screen -d -m npm run start

cd $workingDir
stack run

