import ws = require('ws');
const fs = require('fs-extra');
import path = require('path');
import * as File from './file';
import * as Stats from './stats';
import * as Random from './random'
import {Proxy} from "./browserCreation";

export let activeFollowerGetters: Set<string> = new Set();

async function getAndSendFollowersCount(socket: any, id: string, timeout: number, proxy: Proxy) {

    try {
        await File.acquireMutex(id);
        const usersInfo: Stats.StatsResponse = await Stats.getFollowers(id, proxy);
        File.releaseMutex(id);
        const userJSON: string = JSON.stringify(usersInfo);
        if (activeFollowerGetters.has(id)) {
            console.log(`Stats sent: ${userJSON.slice(0, 150)}`);
            socket.send(Buffer.from(userJSON));
        } else {
            console.log(`Stats didn't send: followers were counted but user isn't in set ${id}`);
        }
    } catch (e) {
        let error: Stats.StatsResponse = {
            inst_id: id,
            error: {
                error_message: e.message,
                error_code: 'OTHER_ERROR_2'
            }
        }
        sendWithLog(socket, error);
    }

    if (activeFollowerGetters.has(id)) {
        setTimeout(getAndSendFollowersCount, Random.getRandomDelay(8000, 20), socket, id, timeout, proxy);
    }
}

export function runStatsServer(server: ws.Server) {
    server.on('connection', function connection(socket) {

        console.log('Stats: connection established');

        socket.onclose = () => {
            console.log('Stats: connection closed');
        }

        socket.on('message', async function incoming(message: Buffer) {
            console.log(`Stats: ${message.toString()}`);
            const request: Stats.StatsRequest = JSON.parse(message.toString());

            switch (request.status) {
                case 'Start': {
                    if (activeFollowerGetters.has(request.inst_id)) {
                        break;
                    }
                    activeFollowerGetters.add(request.inst_id);
                    let timeout: number = 60000;
                    if (request.timeout != undefined) {
                        timeout = request.timeout;
                    }
                    getAndSendFollowersCount(socket, request.inst_id, timeout, request.proxy as Proxy).catch(e => console.log(`Error while getting data: ${e}`));

                }
                    break;
                case 'Stop': {
                    activeFollowerGetters.delete(request.inst_id);
                }
                    break;
                case 'Logout': {
                    try {
                        activeFollowerGetters.delete(request.inst_id);
                        //Говно решение. Возможна ситуация когда посередине что нибудь вклинится
                        if (await File.isUserLoggedInBot(request.inst_id)) {
                            await File.acquireMutex(request.inst_id);
                            await fs.remove(path.resolve(__dirname, path.resolve(__dirname, `cookies/${request.inst_id}`)));
                            File.releaseMutex(request.inst_id);
                            let okResponse: Stats.StatsResponse = {
                                inst_id: request.inst_id,
                            };
                            sendWithLog(socket, okResponse);
                        } else {
                            let errorObj: Stats.StatsResponse = {
                                inst_id: request.inst_id,
                                error: {
                                    error_message: `Error: there's no such user.`,
                                    error_code: 'LOGOUT_NO_USER'
                                }
                            };
                            sendWithLog(socket, errorObj);
                        }
                    } catch (e) {
                        let errorObj: Stats.StatsResponse = {
                            inst_id: request.inst_id,
                            error: {
                                error_message: `Failure during logout: ${e.message}`,
                                error_code: 'LOGOUT_FAILURE'
                            }
                        };
                        sendWithLog(socket, errorObj);
                    }
                }
                    break;
                default: {
                    let errorInfo: Stats.StatsResponse = {
                        inst_id: request.inst_id,
                        error: {
                            error_message: `THERE'S NO SUCH CASE: ${request.status}`,
                            error_code: 'OTHER_ERROR_2',
                        }
                    };
                    sendWithLog(socket, errorInfo);
                }
                    break;
            }
        });
    });
}


function sendWithLog(socket: any, data: object) {
    const dataJSON = JSON.stringify(data);
    console.log(`Stats sent: ${dataJSON}`);
    socket.send(Buffer.from(dataJSON));
}