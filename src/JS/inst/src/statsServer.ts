import ws = require('ws');

const fs = require('fs-extra');
import path = require('path');
import * as File from './file';
import * as Stats from './stats';
import * as Random from './random'
import {Logger} from "./log";

type GetterState = 'Working' | 'Stopping' | 'LoggingOut';


export let activeFollowerGetters: Map<string, GetterState> = new Map();

async function workerHandler(socket: any, id: string, timeout: number, browserData: Stats.BrowserData) {
    try {
        if (activeFollowerGetters.has(id)) {
            const currentState = activeFollowerGetters.get(id);

            if (currentState == 'Working') {
                const usersInfo: Stats.StatsResult = await Stats.getFollowers(id, browserData);
                if (usersInfo.send) {
                    sendWithLog(socket, usersInfo.response);
                } else {
                    sendWithLog(socket, usersInfo.response, false);
                }
                setTimeout(workerHandler, Random.getRandomDelay(timeout, 20), socket, id, timeout, browserData);
            } else if (currentState == 'Stopping') {
                await browserData.browser.close();
                activeFollowerGetters.delete(id);
                File.releaseMutex(id);
                Logger.info(`Getter stopped id: ${id}`);
            } else if (currentState == 'LoggingOut') {
                await browserData.browser.close();
                await deleteFolder(id, true);
                activeFollowerGetters.delete(id);
                Logger.info(`Logged out: ${id}`);
            }
        } else {
            const logicError: Stats.StatsResponse = {
                inst_id: id,
                error: {
                    error_message: `THERE IS NO SUCH USER IN THE MAP`,
                    error_code: 'LOGIC_ERROR',
                }
            };
            sendWithLog(socket, logicError);
        }
    } catch (e) {
        let errorObj: Stats.StatsResponse = {
            inst_id: id,
            error: {
                error_message: e.message,
                error_code: 'OTHER_ERROR_2'
            }
        }
        sendWithLog(socket, errorObj);
    }
}


export function runStatsServer(server: ws.Server) {
    server.on('connection', function connection(socket) {

        Logger.info('Stats: connection established');

        socket.onclose = () => {
            Logger.info('Stats: connection closed');
        }

        socket.on('message', async function incoming(message: Buffer) {
                Logger.info(`Stats: ${message.toString()}`);
                const request: Stats.StatsRequest = JSON.parse(message.toString());

                switch (request.status) {
                    case 'Start': {
                        if (activeFollowerGetters.has(request.inst_id)) {
                            break;
                        }
                        let timeout: number = 30000;
                        if (request.timeout != undefined) {
                            timeout = request.timeout;
                        }

                        const browserCreation: Stats.BrowserCreation = await Stats.getInstPageBrowser(request.inst_id);

                        if (browserCreation.state == 'browser') {
                            activeFollowerGetters.set(request.inst_id, 'Working');
                            await File.acquireMutex(request.inst_id);
                            workerHandler(socket, request.inst_id, timeout, browserCreation.browserData).catch(e => Logger.info(`THIS ERROR SHOULD NEVER OCCUR!!: ${e}`));
                        } else {
                            const errorObj: Stats.StatsResponse = {
                                inst_id: request.inst_id,
                                error: {
                                    error_message: browserCreation.errorMessage,
                                    error_code: browserCreation.errorCode,
                                }
                            };
                            sendWithLog(socket, errorObj);
                        }


                    }
                        break;
                    case 'Stop': {
                        if (activeFollowerGetters.has(request.inst_id)) {
                            activeFollowerGetters.set(request.inst_id, 'Stopping');
                        }
                    }
                        break;
                    case 'Logout': {
                        if (activeFollowerGetters.has(request.inst_id)) {
                            activeFollowerGetters.set(request.inst_id, 'LoggingOut');
                        } else {
                            try {
                                if (await File.isUserLoggedInBot(request.inst_id)) {
                                    await deleteFolder(request.inst_id, false);
                                    const okResponse: Stats.StatsResponse = {
                                        inst_id: request.inst_id,
                                    };
                                    sendWithLog(socket, okResponse);
                                } else {
                                    const errorObj: Stats.StatsResponse = {
                                        inst_id: request.inst_id,
                                        error: {
                                            error_message: `Error: there's no such user.`,
                                            error_code: 'LOGOUT_NO_USER'
                                        }
                                    };
                                    sendWithLog(socket, errorObj);
                                }
                            } catch (e) {
                                const errorObj: Stats.StatsResponse = {
                                    inst_id: request.inst_id,
                                    error: {
                                        error_message: `Failure during logout: ${e.message}`,
                                        error_code: 'LOGOUT_FAILURE'
                                    }
                                };
                                sendWithLog(socket, errorObj);
                            }
                        }
                    }

                        break;
                }
            }
        );
    });
}


async function deleteFolder(id: string, isMutexAcquired: boolean) {
    if (!isMutexAcquired) {
        await File.acquireMutex(id);
    }
    await fs.remove(path.resolve(__dirname, path.resolve(__dirname, `cookies/${id}`)));
    File.releaseMutex(id);
}


function sendWithLog(socket: any, data: object, send = true) {
    const dataJSON = JSON.stringify(data);
    Logger.info(`Stats sent: ${dataJSON.slice(0, 150)}`);
    if (send) {
        socket.send(Buffer.from(dataJSON));
    }
}