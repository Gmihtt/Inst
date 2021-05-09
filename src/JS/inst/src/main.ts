import ws = require('ws');
import {runLoginServer} from "./loginServer";
import {runStatsServer} from "./statsServer";
import {runInfoServer} from "./infoServer";
import fs = require('fs-extra');
import path = require('path');
import {Logger} from "./log";

//Cleaning working dir before launch and ensure that cookies exist
(async () => {
    await fs.remove(path.resolve(__dirname, 'loginDirs'));
    await fs.mkdirp(path.resolve(__dirname, 'loginDirs'));
    // TODO CREATE MUTEXES AFTER THIS
    await fs.mkdirp(path.resolve(__dirname, 'cookies'));
    await fs.mkdirp(path.resolve(__dirname, 'errors'));
    await fs.mkdirp(path.resolve(__dirname, 'statsErrors'));
})().then(
    () => {
        const loginServer = new ws.Server({
            port: 5012,
        });

        const statsServer = new ws.Server({
            port: 5013,
        });

        const infoServer = new ws.Server({
            port: 5014,
        })

        runLoginServer(loginServer);
        runStatsServer(statsServer);
        runInfoServer(infoServer);

        Logger.info('Servers started');
    },
    (error) => {
        Logger.info(`Some error occurred:  ${error.message}. Terminating...`);
    }
);
