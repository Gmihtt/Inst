const WebSocket = require('ws');

const ws = new WebSocket('localhost:5013');

ws.on('open', function open() {
    ws.send(JSON.stringify({id: 14490532747}));
});

ws.on('message', function incoming(data) {
    console.log(data);
});
