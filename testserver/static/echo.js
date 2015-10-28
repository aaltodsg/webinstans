//  var wsUri = "ws://echo.websocket.org/";
var wsUri = "ws://localhost:12345/bongo";
var output;
var graph;
var websocket;

function init()
{
    output = document.getElementById("output");
    graph = document.getElementById("graph");
    testWebSocket();
}

function testWebSocket()
{
    websocket = new WebSocket(wsUri);
    websocket.onopen = function(evt) { onOpen(evt) };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };
}

function onOpen(evt)
{
    writeToScreen("CONNECTED");
    // doSend("dot rules.rq");
}

function onClose(evt)
{
    writeToScreen("DISCONNECTED");
}

function onMessage(evt)
{
    // writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
    //    websocket.close();
    var data = evt.data;
    var i = data.indexOf(" ");
    var cmd = data.substring(0, i);
    writeToScreen('<span style="color: blue;">CMD: ' + cmd+'</span>');
    if (cmd == "dot-result") {
	//         writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
        var graph = data.substring(i);
        // writeToScreen('<span style="color: blue;">GRAPH: ' + graph +'</span>');
        // document.body.innerHTML += Viz(graph);
        document.body.innerHTML += graph;
	//         graph.innerHTML = Viz(graph);
    }
}

function onError(evt)
{
    writeToScreen('<span style="color: red;">ERROR:</span> ' + evt.data);
}

function doSend(message)
{
    writeToScreen("SENT: " + message); 
    websocket.send(message);
}

function writeToScreen(message)
{
    var pre = document.createElement("p");
    pre.style.wordWrap = "break-word";
    pre.innerHTML = message;
    output.appendChild(pre);
}

// -r /Users/enu/aaltodsg/instans/tests/input/exists/simple.rq --rete-html=/Users/enu/aaltodsg/webinstans/testserver/hunch/simple.html

function launchInstans()
{
    parms = $('#parameters').val();
    // alert('launching instans with parameters ' + parms);
    websocket.send('parameters ' + parms);
    // websocket.send('dot');
}

function getDot() {
    websocket.send('dot');
}

window.addEventListener("load", init, false);
