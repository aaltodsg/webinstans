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
var definingNodes = new Object();
var prevNode = 'missing';
var prevOperation = 'missing';
var prevToken = 'missing';
var edgeTraverseInfo = new Object();
var ignoreChecksums = true;
var trackCounter = 0;
var trackEnterOps = [];
var opTrackMapping = [];
var varMappings = new Object();
var reverseVarMappings = new Object();

// Auxiliary functions
function stringBefore(str, delim) {
    return str.substring(0, str.indexOf(delim));
}

function stringAfter(str, delim) {
    return str.substring(str.indexOf(delim)+delim.length);
}

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

var seenMessage = -1;

function onMessage(evt)
{
    // writeToLog('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
    //    websocket.close();
    var data = evt.data;
    var messageNoStr = stringBefore(data, ' ');
    var messageNo = parseInt(messageNoStr);
    if (messageNo != seenMessage + 1) {
	alert('Messages out of sequence! seenMessage = ' + seenMessage + ' received message = ' + messageNo);
    } else {
	seenMessage = messageNo;
    }
    data = stringAfter(data, ' ');
    var cmd = stringBefore(data, ' ');
    var args = stringAfter(data, ' ');
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
    } else if (cmd == "var-mappings") {
	$('#varMappings').css("visibility", "visible");
	$('#varMappings').css("display", "block");
	var mappings = jQuery.parseJSON(args);
	for (var i in mappings) {
	    var mapping = mappings[i];
	    $('#varMappings').append('<div class="varMapping"></div>').find('div:last-child').append(jsonToHTML(mapping[0])).append('<span class="niceToKnow"> (internally ' + jsonToHTML(mapping[1]) + ')</span>');
	    var from = mapping[1]["value"];
	    var to = mapping[0]["value"];
	    // alert('from ' + from + ' to ' + to);
	    if (typeof varMappings[from] != "undefined") {
		alert("Variable " + from + " already mapped to " + varMappings[from]);
	    } else {
		varMappings[from] = to;
	    }
	    if (typeof reverseVarMappings[to] != "undefined") {
		alert("Variable " + to + " already mapped to " + reverseVarMappings[to]);
	    } else {
		reverseVarMappings[to] = from;
	    }
	}
    } else if (cmd == "defining-nodes") {
	var parsedList = jQuery.parseJSON(args);
	for (var i in parsedList) {
	    var item = parsedList[i];
	    var v = item[0];
	    var nodes = item[1];
	    definingNodes[v] = nodes;
	    // alert(v + ' -> ' + nodes.length + ' nodes');
	}
    // } else if (cmd == "enter" || cmd == "exit") {
    // 	$('#ops').css("visibility", "visible");
    // 	$('#ops').css("display", "block");
    // 	var c = $('#ops div').length;
    // 	var indent = null;
    // 	if (cmd == "enter") {
    // 	    indent = new Array((traceLevel+1)*traceLevelIndent).join('&nbsp;')
    // 	    if (traceLevel == 0) {
    // 		trackCounter = trackCounter + 1;
    // 		trackEnterOps.push([]);
    // 		console.log('>>>Increment trackCounter to ' + trackCounter + ' at operation ' + c);
    // 		console.log('>>>trackEnterOps[' + trackCounter + ']' + trackEnterOps[trackCounter]);
    // 	    }
    // 	    traceLevel = traceLevel + 1;
    // 	    trackEnterOps[trackCounter].push(c);
    // 	} else {
    // 	    traceLevel = traceLevel - 1;
    // 	    indent = new Array((traceLevel+1)*traceLevelIndent).join('&nbsp;')
    // 	}
    // 	opTrackMapping.push(trackCounter);
    // 	var operation = stringBefore(args, ' ');
    // 	var params = stringAfter(args, ' ');
    // 	var jsonParams = jQuery.parseJSON(params);
    // 	var content = callToHTML(cmd, operation, jsonParams);
    // 	$('#ops').append('<div id="traceOp' + c + '"class="trace"></div>').find('div:last-child').append(content).prepend(indent).click(function () {
    // 	    // alert('calling makeCurrentOp('+ $('#ops').length + ')');
    // 	    makeCurrentOp(c);
    // 	});
    // 	if (cmd == "enter") {
    // 	    addEdgeTraverseInfo(operation, jsonParams, c);
    // 	}
    } else if (cmd == "trace") {
	var parsedTrace = jQuery.parseJSON(args);
    	$('#ops').css("visibility", "visible");
    	$('#ops').css("display", "block");
	processTrace(parsedTrace);
    } else if (cmd == "end") {
	$('#player').css("visibility", "visible");
	$('#player').css("display", "block");
	var status = stringBefore(args, ' ');
	var rest = stringAfter(args, ' ');
	$('#executionInfo').text('Execution ' + args + '. ' + $('#ops div').length + ' operations');
	if (status == "failed") {
	    $('#executionInfo').addClass('executionFailed');
	}
	// makeCurrentOp(0);
	// showEdgeTraverseInfo();
	// for (var k = 0 ; k <= trackCounter; k++) {
	//     highlightTrackEnterOperations(k);
	// }
    }
}

function processTrace(trace) {
    // $('#ops').append(parsedTrace);
    console.log(trace);
    for (var i in trace) {
	var item = trace[i];
	var cmd = item["direction"];
	var op = item["operation"];
	var parms = item["parameters"];
    	var indent = null;
    	if (cmd == "enter") {
    	    indent = new Array((traceLevel+1)*traceLevelIndent).join('&nbsp;')
    	    if (traceLevel == 0) {
    		trackCounter = trackCounter + 1;
    		trackEnterOps.push([]);
    		console.log('>>>Increment trackCounter to ' + trackCounter + ' at operation ' + i);
    		console.log('>>>trackEnterOps[' + (trackCounter-1) + ']' + trackEnterOps[trackCounter-1]);
    	    }
    	    traceLevel = traceLevel + 1;
    	    trackEnterOps[(trackCounter-1)].push(i);
    	} else {
    	    traceLevel = traceLevel - 1;
    	    indent = new Array((traceLevel+1)*traceLevelIndent).join('&nbsp;')
    	}
	var content = callToHTML(cmd, op, parms);
    	$('#ops').append('<div id="traceOp' + i + '"class="trace"></div>').find('div:last-child').append(content).prepend(indent).click(function () {
    	    makeCurrentOp(i);
    	});
    }
    $('div[class="trace"] span[class="var"]').each(function() {
	var from = $(this).text();
	if (typeof varMappings[from] != "undefined") {
	    $(this).text(varMappings[from]);
	}
    });
}

function getOrInitialize(map, key) {
    if (!map.hasOwnProperty(key)) {
	map[key] = new Object();
    }
    // alert('getOrInitialize ' + key);
    return map[key];
}

var foo = null;

function jsonToString(json) {
}

function addEdgeTraverseInfo(operation, jsonParams, opNo) {
    console.log('addEdgeTraverseInfo(' + operation + ', ' + jsonParams + ', ' + opNo + ')');
    if (operation == 'rete-add' || operation == 'rete-remove') {
	console.log('!!!' + operation);
	prevNode = 'root';
	prevOperation = operation;
	jsonParams.shift();
	var jl = jsonToHTML(jsonParams);
	console.log('jl='+jl);
	var elem = jQuery('<span>'+jl+'</span>');
	prevToken = elem.text();
    } else if (operation == "add-token" || operation == "add-alpha-token" || operation == "add-beta-token" ||
	       operation == "remove-token" || operation == "remove-alpha-token" || operation == "remove-beta-token") {
	var node = jsonParams[0]["value"];
	var elem = jQuery('<span>'+jsonListToHTML(jsonParams[1]["value"])+'</span>');
	var token = elem.text();
	// console.log('token = '+token);
	// console.log('params = ' + jsonParams[1]);
	// console.log(jsonParams[1]["value"]);
	// console.log('elem = ' + elem);
	var map = getOrInitialize(edgeTraverseInfo, prevNode);
	map = getOrInitialize(map, prevOperation);
	map = getOrInitialize(map, prevToken);
	map = getOrInitialize(map, node);
	map = getOrInitialize(map, operation);
	map = getOrInitialize(map, token);
	map[opNo] = jsonParams;
	console.log('prevNode=' + prevNode + ' prevOperation=' + prevOperation + ' prevToken=' + prevToken);
	console.log('node=' + node + ' operation=' + operation + ' token=' + token);
	console.log('result = ' + edgeTraverseInfo[prevNode][prevOperation][prevToken][node][operation][token][opNo]);
	prevNode = node;
	prevOperation = operation;
	prevToken = token;
    }
}

function showEdgeTraverseInfo() {
    console.log('showEdgeTraverseInfo()');
    console.log('trackCounter = ' + trackCounter);
    Object.keys(edgeTraverseInfo).forEach(function(fromNode) {
        console.log('fromNode = ' + fromNode);
	var fromOperationMap = edgeTraverseInfo[fromNode];
	Object.keys(fromOperationMap).forEach(function(fromOperation) {
            console.log('fromOperation = ' + fromOperation);
	    var fromTokenMap = fromOperationMap[fromOperation];
	    Object.keys(fromTokenMap).forEach(function(fromToken) {
		console.log('fromToken = ' + fromToken);
		var toNodeMap = fromTokenMap[fromToken];
		Object.keys(toNodeMap).forEach(function(toNode) {
		    console.log('toNode = ' + toNode);
		    var toOperationMap = toNodeMap[toNode];
		    Object.keys(toOperationMap).forEach(function(toOperation) {
			console.log('toOperation = ' + toOperation);
			var toTokenMap = toOperationMap[toOperation];
			Object.keys(toTokenMap).forEach(function(toToken) {
			    console.log('toToken = ' + toToken);
			    $('#edgeTraverseInfo').append('<div class="edgeTraverse"></div>').find('div:last-child').append(span('edgeFromNode', fromNode)).append('->').append(span('edgeFromOperation', fromOperation)).append('->').append(span('edgeFromToken', htmlEncode(fromToken))).append('->').append(span('edgeToNode', toNode)).append('->').append(span('edgeToOperation', toOperation)).append('->').append(span('edgeToToken', htmlEncode(toToken)));
			});
		    });
		});
	    });
	});
    });
}


function highlightTrackEnterOperations(tn) {
    for (var i in trackEnterOps[tn]) {
	var ops = trackEnterOps[i];
	for (var j in ops) {
	    var op = ops[j];
	    console.log('tn ' + tn + ' op ' + op);
	}
    }
}

// prevNode -> prevOp -> prevToken ->	node -> operation -> token

function span(cls, txt) {
    return '<span class="' + cls + '">' + txt + '</span>';
}

function callToHTML(cmd, operation, jsonParams) {
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
	if (typeof type == 'undefined') {
	    console.log('undefined type in ' + o + ', o is of type ' + (typeof o));
	}
	if (type == "iri") {
	    value = htmlEncode(value);
	}
	// alert('type = ' + type + ', value = ' + value);
	switch (type) {
	case "binding":
	    return '<span class="binding">' + jsonToHTML(o["var"]) + ' = ' + jsonToHTML(o["value"]) + '</span>';
	case "token":
	    return jsonListToHTML(value);
	default:
	    return span(type, value);
	}
    }
}

function nodeHighlightSelector(node) {
    return $('#' + node + ' ellipse');
}

function makeCurrentOp(n) {
    if (currentOp != null) {
	$('#traceOp'+currentOp).removeClass('currentOp');
    }
    currentOp = n;
    var elem = $('#traceOp'+n);
    elem.addClass('currentOp');
    elem[0].scrollIntoView({behavior: "smooth", block: "end"});
    var cmd = $('#traceOp' + n + ' span[class="cmd"]').html();
    var operation = $('#traceOp' + n + ' span[class="function"]').html();
    // alert(operation);
    if (operation == "add-token" || operation == "add-alpha-token" || operation == "add-beta-token" ||
	operation == "remove-token" || operation == "remove-alpha-token" || operation == "remove-beta-token") {
	var node = $('#traceOp' + n + ' span[class="node"]').html();
	// alert(cmd + ' ' + operation + ' in node ' + node);
	if (currentNode) {
	    nodeHighlightSelector(currentNode).css(savedCss[currentNode]);
	}
	currentNode = node;
	savedCss[currentNode] = nodeHighlightSelector(currentNode).css(nodePropNames);
	nodeHighlightSelector(currentNode).css(currentNodeCss(cmd, operation));
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
