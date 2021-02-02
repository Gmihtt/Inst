'use strict';

import ws = require('ws');

import {getFollowers, StatsRequest, StatsResponse} from "./stats";

const server = new ws.Server({
    port: 5013,
});

let activeFollowerGetters: Set<string> = new Set();

async function getAndSendFollowersCount(socket: any, id: string, timeout: number){
    const usersInfo: StatsResponse = await getFollowers(id);
    console.log(JSON.stringify(usersInfo));
    socket.send(Buffer.from(JSON.stringify(usersInfo)));
    if (activeFollowerGetters.has(id)){
        setTimeout(getAndSendFollowersCount, timeout, socket, id, timeout);
    }
}





server.on('connection', function connection(socket){

    console.log('connection established');

    socket.onclose = function (){
        console.log('connection closed');
    }

    socket.on('message', async function incoming(message: Buffer){
        console.log(message.toString());
        const request: StatsRequest = JSON.parse(message.toString());

        if (request.action === 'Start'){
            activeFollowerGetters.add(request.inst_id);
            let timeout: number = 60000;
            if ("timeout" in request){
                timeout = request.timeout as number;
            }
            getAndSendFollowersCount(socket, request.inst_id, timeout).catch(e => console.log(`Error while getting data: ${e}`));
        } else if(request.action === 'Stop'){
            activeFollowerGetters.delete(request.inst_id);
        }
    })
});

//startDebug(1, 'alone.shard', 'Kone4nOG4V92FBf');
//startDebug(3, 'orgazm.exe', 'G4V92FBf');
//startBrowser(4, 'alone.shard', 'Kone4nOG4V92FBf');
//startBrowser(4, 'alone.shard', 'Kone4nOG4V92FBf');
//startBrowser(5, 'orgazm.exe', 'G4V92FBf');
//startBrowser(6, 'mood.to.love', 'Kone4nODurahan');
