import ws = require('ws');

import {Login, LoginRequest, LoginResponse} from "./login";


const doubleAuthLogins = new Map();
const susLogins = new Map();

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

                        sendWithLog(socket, loginInfo);
                    } catch (e) {
                        let errorInfo: LoginResponse = {
                            type: 'Error',
                            username: userData.username,
                            error: "Failure to start/close browser or filesystem failure: " + e.message,
                        }
                        sendWithLog(socket, errorInfo);
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

                        sendWithLog(socket, doubleAuthInfo);
                    } else {
                        let errorInfo: LoginResponse = {
                            type: 'Error',
                            username: userData.username,
                            error: `There's no such username in doubleAuth map -- ${userData.username}`,
                        }
                        sendWithLog(socket, errorInfo);
                    }
                    doubleAuthLogins.delete(userData.username);
                }
                    break;
                case 'Sus':{
                    if (susLogins.has(userData.username)){
                        let login: Login = susLogins.get(userData.username);
                        let susInfo = await login.sus(userData.username, userData.body);

                        sendWithLog(socket, susInfo);
                    } else {
                        let errorInfo: LoginResponse = {
                            type: 'Error',
                            username: userData.username,
                            error: `There's no such username in susLogins map -- ${userData.username}`,
                        }
                        sendWithLog(socket, errorInfo);
                    }
                    susLogins.delete(userData.username);
                }
                    break;
            }
        })
    });
}



function sendWithLog(socket: any, data: object){
    const dataJSON = JSON.stringify(data);
    console.log(`Login sent: ${dataJSON}`);
    socket.send(Buffer.from(dataJSON));
}