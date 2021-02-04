const WebSocket = require('ws');

const ws = new WebSocket('ws://localhost:5013');


ws.on('open', function open() {
    ws.send(JSON.stringify({inst_id: 14490532747, action: 'start'}));
});

/*
ws.on('open', function open() {
    ws.send(JSON.stringify({username: 'orgazm.exe', password: 'G4V92FBf'}));
});
*/
ws.on('message', function incoming(data) {
    console.log(data);
});
