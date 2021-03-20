import ws = require('ws');

import {runLoginServer} from "./loginServer";
import {runStatsServer} from "./statsServer";
import * as Proxy from "./proxyTester";


Proxy.checkProxyAndSetVar().then(
    () => {
        const loginServer = new ws.Server({
            port: 5012,
        });

        const statsServer = new ws.Server({
            port: 5013,
        });
        runLoginServer(loginServer);
        runStatsServer(statsServer);
    },
    (error: Error) => {
        console.log(`Proxy validating error: ${error.message}. Terminating...`);
    }
);
