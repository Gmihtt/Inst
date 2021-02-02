const WebSocket = require('ws');

//const ws = new WebSocket('ws://localhost:5012');
const ws = new WebSocket('localhost:5013');

//orgazm.exe login
/*
ws.on('open', function open() {
    ws.send(JSON.stringify({username: 'orgazm.exe', password: 'G4V92FBf'}));
    //ws.send(JSON.stringify({username: 'alone.shard', password: 'Kone4nOG4V92FBf'}));
});*/

//orgazm.exe stats

ws.on('open', function open() {
    ws.send(JSON.stringify({id: 14490532747}));
});


//alone.shard login
/*ws.on('open', function open() {
    ws.send(JSON.stringify({username: 'alone.shard', password: 'Kone4nOG4V92222222FBf'}));
});*/

//alone.shard stats
/*ws.on('open', function open() {
    ws.send(JSON.stringify({id: 7749310308}));
});*/

ws.on('message', function incoming(data) {
    console.log(data);
});
