//  var wsUri = "ws://echo.websocket.org/";
var wsUri = "ws://localhost:12345/bongo";
var log;
var graph;
var websocket;
var isRunning = false;
var currentNode = null;
var nodePropNames = ['fill', 'stroke'];
var savedCss = {};
var currentOp = null;

function init()
{
    log = document.getElementById("log");
    graph = document.getElementById("graph");
    testWebSocket();
    $('#rewindButton').click(function() {
	makeCurrentOp(0);
    });
    $('#stepBackButton').click(function() {
	if (currentOp) {
	    makeCurrentOp(currentOp - 1);
	}
    });
    $('#stopButton').click(function() {
    });
    $('#playButton').click(function() {
    });
    $('#pauseButton').click(function() {
    });
    $('#stepForwardButton').click(function() {
	if (currentOp < $('#ops div').length - 1) {
	    makeCurrentOp(currentOp + 1);
	}
    });
    $('#endButton').click(function() {
	makeCurrentOp($('#ops div').length - 1);
    });
    $('#initializeInstans').click(function() {
	launchInstans();
    });  
    $('#getDot').click(function() {
	getDot();
    }); 
    $('#runInstans').click(function() {
	runInstans();
    });
    $( document ).tooltip();
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

function currentNodeCss() {
    return {stroke: '#aa0000', fill: '#bb6666'};
}

function onMessage(evt)
{
    // writeToLog('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
    //    websocket.close();
    var data = evt.data;
    var i = data.indexOf(" ");
    var cmd = data.substring(0, i);
    var args = data.substring(i+1);
    writeToLog('<span style="color: blue;">CMD: ' + cmd+'</span>');
    if (cmd == "dot-result") {
	//         writeToLog('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
        var graph = args;
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
    } else if (cmd == "enter" || cmd == "exit") {
	var c = $('#ops div').length;
	$('#ops').append('<div id="traceOp' + c + '"class="trace"></div>').find('div:last-child').text(data).click(function () {
	    // alert('calling makeCurrentOp('+ $('#ops').length + ')');
	    makeCurrentOp(c);
	});
	// var j = args.indexOf(" ");
	// var operation = args.substring(0, j);
	// var rest = args.substring(j+1);
	// $('#ops').append('<div class="trace"></div>').find('div:last-child').text(data).click(function () {
	//     if (operation == "add-token" || operation == "add-alpha-token" || operation == "add-beta-token" ||
	// 	operation == "remove-token" || operation == "remove-alpha-token" || operation == "remove-beta-token") {
	// 	var k = rest.indexOf(" ");
	// 	var node = rest.substring(0, k);
	// 	// alert(cmd + ' ' + operation + ' in node ' + node);
	// 	if (currentNode) {
	// 	    $('#' + currentNode + ' ellipse').css(savedCss[currentNode]);
	// 	}
	// 	currentNode = node;
	// 	savedCss[currentNode] = $('#' + currentNode + ' ellipse').css(nodePropNames);
	// 	$('#' + currentNode + ' ellipse').css(currentNodeCss());
	//     }
	// });
    }
}

function makeCurrentOp(n) {
    currentOp = n;
    var elem = $('#traceOp'+n);
    var data = elem.text();
    // alert('called makeCurrentOp('+n+ '), data=' + data);
    var i = data.indexOf(" ");
    var cmd = data.substring(0, i);
    var args = data.substring(i+1);
    var j = args.indexOf(" ");
    var operation = args.substring(0, j);
    var rest = args.substring(j+1);
    if (operation == "add-token" || operation == "add-alpha-token" || operation == "add-beta-token" ||
	operation == "remove-token" || operation == "remove-alpha-token" || operation == "remove-beta-token") {
	var k = rest.indexOf(" ");
	var node = rest.substring(0, k);
	// alert(cmd + ' ' + operation + ' in node ' + node);
	if (currentNode) {
	    $('#' + currentNode + ' ellipse').css(savedCss[currentNode]);
	}
	currentNode = node;
	savedCss[currentNode] = $('#' + currentNode + ' ellipse').css(nodePropNames);
	$('#' + currentNode + ' ellipse').css(currentNodeCss());
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
    var pre = document.createElement("div");
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
