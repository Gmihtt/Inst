'use strict'

import ws = require('ws');
import fs = require('fs-extra');
import path = require('path');

import {login, LoginRequest, LoginResponse} from "./login";


//Cleaning working folder before launch and ensure that cookies exist
(async () => {
    await fs.remove(path.resolve(__dirname, 'loginDirs'));
    await fs.mkdirp(path.resolve(__dirname, 'loginDirs'));
    await fs.mkdirp(path.resolve(__dirname, 'cookies'));
})();
const server = new ws.Server({
    port: 5012,
});


server.on('connection', function connection(socket){
    console.log('Login: connection established');

    socket.onclose = function(){
        console.log('Login: connection finished');
    }
    socket.on('message', async function incoming(message: Buffer){
        console.log(`Login: ${message.toString()}`);
        const userData: LoginRequest = JSON.parse(message.toString());
        try {
            const loginInfo: LoginResponse = await login(userData.username, userData.password);
            const loginJSON = JSON.stringify(loginInfo);
            console.log(`Login: Data sent: ${loginJSON}`);
            socket.send(Buffer.from(loginJSON));
        } catch(e) {
            let errorInfo: LoginResponse = {
                status: false,
                username: userData.username,
                error_message: "Failure to start browser: " + e.message,
            }
            const errorJSON = JSON.stringify(errorInfo);
            console.log(`Login: Error sent: ${errorJSON}`);
            socket.send(Buffer.from(errorJSON));
        }
    })
});


