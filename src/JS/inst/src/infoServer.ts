import ws = require('ws');
import * as StatsServer from "./statsServer";
import {Logger} from "./log";

interface UserInfo {
    id: string
    is_active: boolean
}

interface AllRequest{
    status: 'AllStatus'
    admin_id: string,
}

interface OtherRequest{
    status: 'UserStatus' | 'GroupStatus'
    admin_id: string
    inst_ids: Array<string>
}

type InfoRequest = AllRequest | OtherRequest

type Response = 'User' | 'Group' | 'All'

interface InfoResponse {
    status: Response
    admin_id: string
    users_info: Array<UserInfo>
    user_count_active?: number
}

export function runInfoServer(server: ws.Server) {
    server.on('connection', function connection(socket) {

        Logger.info('Info: connection established');

        socket.onclose = () => {
            Logger.info('Info: connection closed');
        }

        socket.on('message', async function incoming(message: Buffer) {
            Logger.info(`Info: ${message.toString()}`);
            const request: InfoRequest = JSON.parse(message.toString());

            switch (request.status) {
                case 'UserStatus': {
                    let result = getUserInfoData(request.inst_ids?.[0], request.admin_id);
                    sendWithLog(socket, result);
                }
                    break;
                case 'GroupStatus': {
                    let result = getGroupInfoData(request.inst_ids, request.admin_id);
                    sendWithLog(socket, result);
                }
                    break;
                case 'AllStatus': {
                    let result = getAllInfoData(request.admin_id);
                    sendWithLog(socket, result);
                }
            }


        });

    });
}


function getUserInfoData(user: string, admin: string): InfoResponse {
    return {
        status: 'User',
        admin_id: admin,
        users_info: [{
            id: user,
            is_active: StatsServer.activeFollowerGetters.has(user),
        }],
    };
}

function getGroupInfoData(group: Array<string>, admin: string): InfoResponse {
    const usersInfo: Array<UserInfo> = [];
    let activeUsers: number = 0;
    for (let user of group) {
        const isActive: boolean = StatsServer.activeFollowerGetters.has(user);
        if (isActive) {
            activeUsers++;
        }
        usersInfo.push({
            id: user,
            is_active: isActive,
        });
    }
    return {
        status: 'Group',
        admin_id: admin,
        users_info: usersInfo,
        user_count_active: activeUsers,
    };
}

function getAllInfoData(admin: string): InfoResponse {
    const usersInfo: Array<UserInfo> = [];
    let activeUsers: number = 0;
    for (let user of StatsServer.activeFollowerGetters.keys()) {
        activeUsers++;
        usersInfo.push({
            id: user,
            is_active: true,
        });
    }
    return {
        status: 'All',
        admin_id: admin,
        users_info: usersInfo,
        user_count_active: activeUsers,
    };
}

function sendWithLog(socket: any, data: object) {
    const dataJSON = JSON.stringify(data);
    Logger.info(`Stats sent: ${dataJSON}`);
    socket.send(Buffer.from(dataJSON));
}