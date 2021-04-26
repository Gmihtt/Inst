import ws = require('ws');
import {runLoginServer} from "./loginServer";
import {runStatsServer} from "./statsServer";
import {runInfoServer} from "./infoServer";
import * as Proxy from "./proxyTester";
import fs = require('fs-extra');
import path = require('path');

//Cleaning working dir before launch and ensure that cookies exist
(async () => {
    await fs.remove(path.resolve(__dirname, 'loginDirs'));
    await fs.mkdirp(path.resolve(__dirname, 'loginDirs'));
    // TODO CREATE MUTEXES AFTER THIS
    await fs.mkdirp(path.resolve(__dirname, 'cookies'));
    await fs.mkdirp(path.resolve(__dirname, 'errors'));
    await fs.mkdirp(path.resolve(__dirname, 'statsErrors'));
})().then(Proxy.checkProxyAndSetVar).then(
    () => {
        console.log("Proxy OK");

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
    }
).catch((error: Error) => {
    console.log(`Some error occurred:  ${error.message}. Terminating...`);
});
