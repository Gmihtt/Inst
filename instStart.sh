#!/bin/bash
workingDir=$(pwd)

cd src/APP/Scripts/JS/inst
pwd
screen -d -m npm run login
screen -d -m npm run start

sleep 1s

cd $workingDir
stack run

