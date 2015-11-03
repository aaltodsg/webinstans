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
var traceLevel = 0;
var traceLevelIndent = 2;

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

function currentNodeCss(command, operation) {
    if (command == "enter") {
	return {stroke: '#aa0000', fill: '#bb6666'};
    } else {
	return {stroke: '#00aa00', fill: '#66bb66'};
    }
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
	$('#graph').css("display", "block");
    } else if (cmd == "enter" || cmd == "exit") {
	$('#ops').css("visibility", "visible");
	$('#ops').css("display", "block");
	var c = $('#ops div').length;
	var indent = null;
	if (cmd == "enter") {
	    indent = new Array((traceLevel+1)*traceLevelIndent).join('&nbsp;')
	    traceLevel = traceLevel + 1;
	} else {
	    traceLevel = traceLevel - 1;
	    indent = new Array((traceLevel+1)*traceLevelIndent).join('&nbsp;')
	}
	var content = callToHTML(cmd, args);
	$('#ops').append('<div id="traceOp' + c + '"class="trace"></div>').find('div:last-child').append(content).prepend(indent).click(function () {
	    // alert('calling makeCurrentOp('+ $('#ops').length + ')');
	    makeCurrentOp(c);
	});
    } else if (cmd == "end") {
	$('#player').css("visibility", "visible");
	$('#player').css("display", "block");
	var j = args.indexOf(" ");
	var status = args.substring(0, j);
	var rest = args.substring(j+1);
	$('#executionInfo').text('Execution ' + args + '. ' + $('#ops div').length + ' operations');
	if (status == "failed") {
	    $('#executionInfo').addClass('executionFailed');
	}
	makeCurrentOp(0);
    }
}

function span(cls, txt) {
    return '<span class="' + cls + '">' + txt + '</span>';
}

function callToHTML(cmd, args) {
    var j = args.indexOf(" ");
    var operation = args.substring(0, j);
    var params = args.substring(j+1);
    var jsonParams = jQuery.parseJSON(params);
    // alert(jsonParams);
    return span("cmd", cmd) + '&nbsp;' + span("function", operation) + '&nbsp;' + jsonListToHTML(jsonParams, open='(', close=')');
}

function jsonListToHTML(list, open='[', close=']', separator=', ') {
    var converted = []
    for (var i in list) {
	var o = list[i];
	converted.push(jsonToHTML(o));
    }
    var result = '<span class="listOpen">' + open + '</span>' + converted.join(separator) + '<span class="listClose">' + close + '</span>';
    // alert(result);
    return result;
}

function htmlEncode(string)
{
  var elem = document.createElement("div");
  elem.innerText = elem.textContent = string;
  string = elem.innerHTML;
  return string;
}

function jsonToHTML(o) {
    if ($.isArray(o)) {
	return jsonListToHTML(o);
    } else {
	var type = o["type"];
	var value = o["value"];
	if (type == "iri") {
	    value = htmlEncode(value);
	}
	// alert('type = ' + type + ', value = ' + value);
	switch (type) {
	case "binding":
	    return '<span class="binding"><span class="var">' + jsonToHTML(o["var"]) + ' = ' + jsonToHTML(o["value"]) + '</span>';
	case "token":
	    return jsonListToHTML(value);
	default:
	    return span(type, value);
	}
    }
}

function makeCurrentOp(n) {
    if (currentOp != null) {
	$('#traceOp'+currentOp).removeClass('currentOp');
    }
    currentOp = n;
    var elem = $('#traceOp'+n);
    elem.addClass('currentOp');
    elem[0].scrollIntoView({behavior: "smooth", block: "end"});
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
	$('#' + currentNode + ' ellipse').css(currentNodeCss(cmd, operation));
    }
}

function onError(evt)
{
    writeToLog('<div style="color: red;">ERROR:</div> ' + evt.data);
    $('#executionStatus').text("Execution failed");
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
