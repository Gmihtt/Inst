import ws = require('ws');

import {runLoginServer} from "./loginServer";
import {runStatsServer} from "./statsServer";

const loginServer = new ws.Server({
    port: 5012,
});

const statsServer = new ws.Server({
    port: 5013,
});


runLoginServer(loginServer);
runStatsServer(statsServer);