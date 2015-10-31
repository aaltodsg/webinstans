//  var wsUri = "ws://echo.websocket.org/";
var wsUri = "ws://localhost:12345/bongo";
var log;
var graph;
var websocket;
var isRunning = false;

function init()
{
    log = document.getElementById("log");
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
    writeToLog("CONNECTED");
    // doSend("dot rules.rq");
}

function onClose(evt)
{
    writeToLog("DISCONNECTED");
}

function onMessage(evt)
{
    // writeToLog('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
    //    websocket.close();
    var data = evt.data;
    var i = data.indexOf(" ");
    var cmd = data.substring(0, i);
    writeToLog('<span style="color: blue;">CMD: ' + cmd+'</span>');
    if (cmd == "dot-result") {
	//         writeToLog('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
        var graph = data.substring(i);
        // writeToLog('<span style="color: blue;">GRAPH: </span>');
        // document.body.innerHTML += Viz(graph);
	$('#graph').html(graph);
        // document.body.innerHTML += graph;
	//         graph.innerHTML = Viz(graph);
	var height = $('#graph').height();
	var width = $('#graph').width();
	var margin = 2;
	$('#graph').height(height+2*margin);
	$('#graph').width(width+2*margin);
        $('#graph' ).scrollTop( 0 );
        $('#graph' ).scrollLeft( 0 );
	$('#graph').css("visibility", "visible");
    } else if (cmd == "enter") {
	$('#ops').append('<div class="op enter"></div>').find('div:last-child').text(data);
    } else if (cmd == "exit") {
	$('#ops').append('<div class="op exit"></div>').find('div:last-child').text(data);
    }
}

function onError(evt)
{
    writeToLog('<div style="color: red;">ERROR:</div> ' + evt.data);
}

function doSend(message)
{
    writeToLog("SENT: " + message); 
    websocket.send(message);
}

function writeToLog(message)
{
    var pre = document.createElement("p");
    pre.style.wordWrap = "break-word";
    pre.innerHTML = message;
    log.appendChild(pre);
}

// -d /Users/enu/aaltodsg/instans/tests/input/exists -r simple.rq --input=simple.trig --select-output-output=simple-output.csv

function launchInstans()
{
    parms = $('#parameters').val();
    // alert('launching instans with parameters ' + parms);
    websocket.send('parameters ' + parms);
    // websocket.send('dot');
}

function runInstans()
{
    if (isRunning) {
	alert('Already running');
    } else {
	isRunning = true;
	websocket.send('run');
    }
}

function getDot() {
    websocket.send('dot');
}

window.addEventListener("load", init, false);
