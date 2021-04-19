import ws = require('ws');

import {Login, LoginRequest, LoginResponse} from "./login";


let doubleAuthLogins = new Map();
let susLogins = new Map();

export function runLoginServer(server: ws.Server) {
    server.on('connection', function connection(socket) {
        console.log('Login: connection established');

        socket.onclose = function () {
            console.log('Login: connection finished');
        }

        socket.on('message', async function incoming(message: Buffer) {
            console.log(`Login: ${message.toString()}`);
            const userData: LoginRequest = JSON.parse(message.toString());
            switch (userData.type) {
                case 'Login': {
                    try {
                        let browserData = await Login.getBrowserAndPage();
                        let login = new Login(browserData);

                        const loginInfo: LoginResponse = await login.login(userData.username, userData.body);

                        if (loginInfo.type === 'DoubleAuth') {
                            doubleAuthLogins.set(loginInfo.username, login);
                        } else if (loginInfo.type === 'Sus') {
                            susLogins.set(loginInfo.username, login);
                        }

                        const loginJSON = JSON.stringify(loginInfo);
                        console.log(`Login login sent: ${loginJSON}`);
                        socket.send(Buffer.from(loginJSON));
                    } catch (e) {
                        let errorInfo: LoginResponse = {
                            type: 'Error',
                            username: userData.username,
                            error: "Failure to start/close browser or filesystem failure: " + e.message,
                        }
                        const errorJSON = JSON.stringify(errorInfo);
                        console.log(`Login login sent: ${errorJSON}`);
                        socket.send(Buffer.from(errorJSON));
                    }
                }
                    break;
                case 'DoubleAuth': {
                    if (doubleAuthLogins.has(userData.username)) {
                        let login: Login = doubleAuthLogins.get(userData.username);
                        let doubleAuthInfo = await login.doubleAuth(userData.username, userData.body);

                        if (doubleAuthInfo.type === 'Sus') {
                            susLogins.set(doubleAuthInfo.username, login);
                        }

                        const doubleAuthJSON = JSON.stringify(doubleAuthInfo);
                        console.log(`Login doubleAuth sent: ${doubleAuthJSON}`);
                        socket.send(Buffer.from(doubleAuthJSON));
                    } else {
                        let errorInfo: LoginResponse = {
                            type: 'Error',
                            username: userData.username,
                            error: `There's no such username in doubleAuth map -- ${userData.username}`,
                        }
                        const errorJSON = JSON.stringify(errorInfo);
                        console.log(`Login doubleAuth sent: ${errorJSON}`);
                        socket.send(Buffer.from(errorJSON));
                    }
                    doubleAuthLogins.delete(userData.username);
                }
                    break;
                case 'Sus':{
                    if (susLogins.has(userData.username)){
                        let login: Login = susLogins.get(userData.username);
                        let susInfo = await login.sus(userData.username, userData.body);

                        const result = JSON.stringify(susInfo);
                        console.log(`Login sus sent: ${result}`);
                        socket.send(Buffer.from(result));
                    } else {
                        let errorInfo: LoginResponse = {
                            type: 'Error',
                            username: userData.username,
                            error: `There's no such username in susLogins map -- ${userData.username}`,
                        }
                        const errorJSON = JSON.stringify(errorInfo);
                        console.log(`Login sus sent: ${errorJSON}`);
                        socket.send(Buffer.from(errorJSON));
                    }
                    susLogins.delete(userData.username);
                }
                    break;
            }
        })
    });

}
