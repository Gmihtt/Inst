import ws = require('ws');

const fs = require('fs-extra');
import path = require('path');

import * as File from "./file";

import * as Stats from "./stats";


let activeFollowerGetters: Set<string> = new Set();

async function getAndSendFollowersCount(socket: any, id: string, timeout: number) {

    try {
        await File.acquireMutex(id);
        const usersInfo: Stats.StatsResponse = await Stats.getFollowers(id);
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
            status: false,
            inst_id: id,
            errorMessage: e.message,
        }
        sendWithLog(socket, error);
    }

    if (activeFollowerGetters.has(id)) {
        setTimeout(getAndSendFollowersCount, timeout, socket, id, timeout);
    }
}

export function runStatsServer(server: ws.Server) {
    server.on('connection', function connection(socket) {

        console.log('Stats: connection established');

        socket.onclose = function () {
            console.log('Stats: connection closed');
        }

        socket.on('message', async function incoming(message: Buffer) {
            console.log(`Stats: ${message.toString()}`);
            const request: Stats.StatsRequest = JSON.parse(message.toString());

            switch (request.action) {
                case 'Start': {
                    if (activeFollowerGetters.has(request.inst_ids?.[0] as string)) {
                        break;
                    }
                    activeFollowerGetters.add(request.inst_ids?.[0] as string);
                    let timeout: number = 60000;
                    if (request.timeout != undefined) {
                        timeout = request.timeout;
                    }
                    getAndSendFollowersCount(socket, request.inst_ids?.[0] as string, timeout).catch(e => console.log(`Error while getting data: ${e}`));

                }
                    break;
                case 'Stop': {
                    activeFollowerGetters.delete(request.inst_ids?.[0] as string);
                }
                    break;
                case 'Logout': {
                    try {
                        activeFollowerGetters.delete(request.inst_ids?.[0] as string);
                        //Говно решение. Возможна ситуация когда посередине что нибудь вклинится
                        if (await File.isUserLoggedInBot(request.inst_ids?.[0] as string)) {
                            await File.acquireMutex(request.inst_ids?.[0] as string);
                            await fs.remove(path.resolve(__dirname, path.resolve(__dirname, `cookies/${request.inst_ids?.[0]}`)));
                            File.releaseMutex(request.inst_ids?.[0] as string);
                            let okResponse: Stats.StatsResponse = {
                                status: true,
                                inst_id: request.inst_ids?.[0] as string,
                            };
                            sendWithLog(socket, okResponse);
                        } else {
                            let errorObj: Stats.StatsResponse = {
                                status: false,
                                inst_id: request.inst_ids?.[0] as string,
                                errorMessage: `Error: there's no such user.`,
                            };
                            sendWithLog(socket, errorObj);
                        }
                    } catch (e) {
                        let errorObj: Stats.StatsResponse = {
                            status: false,
                            inst_id: request.inst_ids?.[0]as string,
                            errorMessage: `Failure during logout: ${e.message}`,
                        };
                        sendWithLog(socket, errorObj);
                    }
                }
                    break;
                case 'UserStatus': {
                    let result = getUserInfoData(request.inst_ids?.[0] as string);
                    sendWithLog(socket, result);
                }
                    break;
                case 'GroupStatus': {
                    let result = getGroupInfoData(request.inst_ids as Array<string>)
                    sendWithLog(socket, result);
                }
                    break;
                case 'AllStatus':
                    let result = getAllInfoData();
                    sendWithLog(socket, result);
            }
        })
    });
}


function getUserInfoData(user: string): Stats.StatusResponse {
    return {
        type: 'User',
        users: [{
            id: user,
            is_active: activeFollowerGetters.has(user),
        }],
    };
}

function getGroupInfoData(group: Array<string>): Stats.StatusResponse{
    let usersInfo: Array<Stats.UserInfo> = [];
    let activeUsers: number = 0;
    for (let user of group) {
        let isActive: boolean = activeFollowerGetters.has(user);
        if (isActive) {
            activeUsers++;
        }
        usersInfo.push({
            id: user,
            is_active: isActive,
        });
    }
    return {
        type: 'Group',
        users: usersInfo,
        count_active: activeUsers,
    };
}

function getAllInfoData(): Stats.StatusResponse{
    let usersInfo: Array<Stats.UserInfo> = [];
    let activeUsers: number = 0;
    for (let user of activeFollowerGetters){
        activeUsers++;
        usersInfo.push({
            id: user,
            is_active: true,
        });
    }
    return {
        type: 'All',
        users: usersInfo,
        count_active: activeUsers,
    };
}

function sendWithLog(socket: any, data: object){
    const dataJSON = JSON.stringify(data);
    console.log(`Stats sent: ${dataJSON}`);
    socket.send(Buffer.from(dataJSON));
}