'use strict';

import ws = require('ws');

import {getFollowers, StatsRequest, StatsResponse} from "./stats";

const server = new ws.Server({
    port: 5013,
});

let activeFollowerGetters: Set<string> = new Set();

async function getAndSendFollowersCount(socket: any, id: string, timeout: number) {
    try {
        const usersInfo: StatsResponse = await getFollowers(id);
        const userJSON: string = JSON.stringify(usersInfo);
        console.log(`Stats: Data sent: ${userJSON}`)
        socket.send(Buffer.from(userJSON));
    } catch (e) {
        const errorJSON: string = JSON.stringify(e.message);
        console.log(`Stats: Error sent: ${errorJSON}`);
        socket.send(Buffer.from(errorJSON));
    }

    if (activeFollowerGetters.has(id)) {
        setTimeout(getAndSendFollowersCount, timeout, socket, id, timeout);
    }
}


server.on('connection', function connection(socket) {

    console.log('Stats: connection established');

    socket.onclose = function () {
        console.log('Stats: connection closed');
    }

    socket.on('message', async function incoming(message: Buffer) {
        console.log(`Stats: ${message.toString()}`);
        const request: StatsRequest = JSON.parse(message.toString());

        switch (request.action) {
            case 'Start':
                if (activeFollowerGetters.has(request.inst_id)){
                    break;
                }
                activeFollowerGetters.add(request.inst_id);
                let timeout: number = 60000;
                if (request.timeout != undefined) {
                    timeout = request.timeout;
                }
                getAndSendFollowersCount(socket, request.inst_id, timeout).catch(e => console.log(`Error while getting data: ${e}`));
                break;
            case 'Stop':
                activeFollowerGetters.delete(request.inst_id);
                break;
            case 'Logout':
                break;
        }
    })
});
