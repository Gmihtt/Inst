import ws = require('ws');

import {Login, LoginRequest, LoginResponse} from "./login";
import {Proxy} from "./browserCreation";


const doubleAuthLogins = new Map();
const susLogins = new Map();
const phoneCheckLogins = new Map();

export function runLoginServer(server: ws.Server) {
    server.on('connection', function connection(socket) {
        console.log('Login: connection established');

        socket.onclose = () => {
            console.log('Login: connection finished');
        }

        socket.on('message', async function incoming(message: Buffer) {
            console.log(`Login: ${message.toString()}`);
            const userData: LoginRequest = JSON.parse(message.toString());
            switch (userData.status) {
                case 'Login': {
                    try {
                        console.log('start');
                        let browserData = await Login.getBrowserAndPage(userData.proxy as Proxy);
                        console.log('got browser');
                        let login = new Login(browserData);
                        console.log('after');
                        const loginInfo: LoginResponse = await login.login(userData.username, userData.body);

                        if (loginInfo.status === 'DoubleAuth') {
                            doubleAuthLogins.set(loginInfo.username, login);
                        } else if (loginInfo.status === 'Sus') {
                            susLogins.set(loginInfo.username, login);
                        } else if (loginInfo.status === 'PhoneCheck') {
                            phoneCheckLogins.set(loginInfo.username, login);
                        }

                        sendWithLog(socket, loginInfo);
                    } catch (e) {
                        // TODO DELETE USER DIR
                        let errorInfo: LoginResponse = {
                            status: 'Error',
                            username: userData.username,
                            error_message: "Failure to start/close browser or filesystem failure: " + e.message,
                        }
                        sendWithLog(socket, errorInfo);
                    }
                }
                    break;
                case 'DoubleAuth': {
                    if (doubleAuthLogins.has(userData.username)) {
                        let login: Login = doubleAuthLogins.get(userData.username);
                        let doubleAuthInfo = await login.doubleAuth(userData.username, userData.body);

                        if (doubleAuthInfo.status === 'Sus') {
                            susLogins.set(doubleAuthInfo.username, login);
                        } else if (doubleAuthInfo.status === 'PhoneCheck') {
                            phoneCheckLogins.set(doubleAuthInfo.username, login);
                        }

                        sendWithLog(socket, doubleAuthInfo);
                    } else {
                        let errorInfo: LoginResponse = {
                            status: 'Error',
                            username: userData.username,
                            error_message: `There's no such username in doubleAuth map -- ${userData.username}`,
                        };
                        sendWithLog(socket, errorInfo);
                    }
                    doubleAuthLogins.delete(userData.username);
                }
                    break;
                case 'Sus': {
                    if (susLogins.has(userData.username)) {
                        let login: Login = susLogins.get(userData.username);
                        let susInfo = await login.sus(userData.username, userData.body);

                        if (susInfo.status === 'PhoneCheck') {
                            phoneCheckLogins.set(susInfo.username, login);
                        }

                        sendWithLog(socket, susInfo);
                    } else {
                        let errorInfo: LoginResponse = {
                            status: 'Error',
                            username: userData.username,
                            error_message: `There's no such username in susLogins map -- ${userData.username}`,
                        };
                        sendWithLog(socket, errorInfo);
                    }
                    susLogins.delete(userData.username);
                }
                    break;
                case 'PhoneCheck': {
                    if (phoneCheckLogins.has(userData.username)) {
                        let login: Login = phoneCheckLogins.get(userData.username);
                        let phoneCheckInfo = await login.phoneCheck(userData.username, userData.body);

                        if (phoneCheckInfo.status === 'Sus') {
                            susLogins.set(phoneCheckInfo.username, login);
                        }

                        sendWithLog(socket, phoneCheckInfo);
                    } else {
                        let errorInfo: LoginResponse = {
                            status: 'Error',
                            username: userData.username,
                            error_message: `There's no such username in phoneCheck map -- ${userData.username}`,
                        };
                        sendWithLog(socket, errorInfo);
                    }
                }
                    break;
                default: {
                    let errorInfo: LoginResponse = {
                        status: 'Error',
                        username: userData.username,
                        error_message: `THERE'S NO SUCH CASE: ${userData.status}`
                    };
                    sendWithLog(socket, errorInfo);
                }
                    break;
            }
        })
    });
}


function sendWithLog(socket: any, data: object) {
    const dataJSON = JSON.stringify(data);
    console.log(`Login sent: ${dataJSON}`);
    socket.send(Buffer.from(dataJSON));
}