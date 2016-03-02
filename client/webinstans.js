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
var parsedTrace = null;
var traceLevel = 0;
var traceLevelIndent = 2;
var definingNodes = new Object();
var usingNodes = new Object();
var prevNode = 'missing';
var prevOperation = 'missing';
var prevToken = 'missing';
var edgeTraverseInfo = new Object();
var ignoreChecksums = true;
var trackCounter = 0;
var trackEnterOps = [];
var opTrackMapping = [];
var varNumericToSymbolicMapping = new Object();
var varSymbolicToNumericMapping = new Object();
var tokenStores = {};
var nodeState = {};

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
    $(function() {
	$( "#runContainer" ).accordion({
	    collapsible: true,
	    active: false
	});
    }); 
    $(function() {
	$( "#logContainer" ).accordion({
	    collapsible: true,
	    active: false
	});
    }); 
    $(function() {
	$( "#varContainer" ).accordion({
	    collapsible: true,
	    active: false
	});
    }); 
    graph = document.getElementById("graph");
    initWebSocket();
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
    // $('#getDot').click(function() {
    // 	getDot();
    // }); 
    $('#runInstans').click(function() {
	runInstans();
    });
    // $( "#varPopupMenuDialog" ).dialog({
    // 	dialogClass: "no-close",
    // 	autoOpen: false,
    // 	show: {
    //         effect: "blind",
    //         duration: 1000
    // 	},
    // 	hide: {
    //         effect: "explode",
    //         duration: 1000
    // 	}
    // });
    // $( "#varPopupMenu" ).menu();
    // $( document ).tooltip();
    // $( document ).tooltip({
    // 	items: '*:not(.ui-dialog-titlebar-close)'
    // });
    // showElement('#varInfo', false);
    showElement('#varMenu', false);
}

function initWebSocket()
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

function showElement(selector, on) {
    if (on) {
	$(selector).css("visibility", "visible");
	$(selector).css("display", "block");
    } else {
	$(selector).css("visibility", "hidden");
	$(selector).css("display", "none");
    }
}

function escapeHtml(str) {
    var div = document.createElement('div');
    div.appendChild(document.createTextNode(str));
    return div.innerHTML;
}


function onMessage(evt)
{
    var data = evt.data;
    var maxlen = 100;
    if (data.length > maxlen) {
	writeToLog('<span style="color: green;">Message begins: ' + escapeHtml(data.substr(0, maxlen))+' ...</span>');
    } else {
	writeToLog('<span style="color: green;">Message begins: ' + escapeHtml(data)+'</span>');
    }
    //    websocket.close();
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
    writeToLog('<span style="color: brown;">Trying to dispatch ' + cmd+'</span>');
    if (cmd == "dot-result") {
	writeToLog('<span style="color: blue;">Dot-result </span>');
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
	showElement('#graph', true);
    } else if (cmd == "var-mappings") {
	writeToLog('<span style="color: blue;">Var-mappings</span>');
	var mappings = jQuery.parseJSON(args);
	for (var i in mappings) {
	    var mapping = mappings[i];
	    // $('#varNumericToSymbolicMapping').append('<div class="varMapping"></div>').find('div:last-child').append(jsonToHTML(mapping[0])).append('<span class="niceToKnow"> (internally ' + jsonToHTML(mapping[1]) + ')</span>');
	    var fromVarHtml = jsonToHTML(mapping[0]);
	    $('#varNumericToSymbolicMapping').append('<li>' + fromVarHtml + '<ul><li class="define-var">Show nodes defining ' + fromVarHtml + '</li><li class="use-var">Show nodes using ' + fromVarHtml + '</ul></li>');
	    var from = mapping[1]["value"];
	    var to = mapping[0]["value"];
	    // alert('from ' + from + ' to ' + to);
	    if (typeof varNumericToSymbolicMapping[from] != "undefined") {
		alert("Variable " + from + " already mapped to " + varNumericToSymbolicMapping[from]);
	    } else {
		varNumericToSymbolicMapping[from] = to;
	    }
	    if (typeof varSymbolicToNumericMapping[to] != "undefined") {
		alert("Variable " + to + " already mapped to " + varSymbolicToNumericMapping[to]);
	    } else {
		varSymbolicToNumericMapping[to] = from;
	    }
	}
	showElement('#varNumericToSymbolicMapping', true);
	// showElement('#varInfo', true);
    } else if (cmd == "defining-nodes") {
	writeToLog('<span style="color: blue;">Defining-nodes</span>');
	var parsedList = jQuery.parseJSON(args);
	for (var i in parsedList) {
	    var item = parsedList[i];
	    var v = item[0];
	    var nodes = item[1];
	    definingNodes[v] = nodes;
	    // alert(v + ' -> ' + nodes.length + ' nodes');
	}
    } else if (cmd == "using-nodes") {
	writeToLog('<span style="color: blue;">Using-nodes</span>');
	var parsedList = jQuery.parseJSON(args);
	for (var i in parsedList) {
	    var item = parsedList[i];
	    var v = item[0];
	    var nodes = item[1];
	    usingNodes[v] = nodes;
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
	writeToLog('<span style="color: blue;">Trace</span>');
	console.log('parsing args');
	console.log(args);
	parsedTrace = jQuery.parseJSON(args);
	console.log(parsedTrace);
	showElement('#ops', true);
	processTrace(parsedTrace);
    } else if (cmd == "end") {
	writeToLog('<span style="color: blue;">End</span>');
	showElement('#player', true);
	var status = stringBefore(args, ' ');
	var rest = stringAfter(args, ' ');
	$('#executionInfo').text('Execution ' + args + '. ' + $('#ops div').length + ' operations');
	if (status == "failed") {
	    $('#executionInfo').addClass('executionFailed');
	}
	makeCurrentOp(0);
	// showEdgeTraverseInfo();
	// for (var k = 0 ; k <= trackCounter; k++) {
	//     highlightTrackEnterOperations(k);
	// }
	$('.var').click(function() {
	    showVarPopupMenuDialog($(this).text());
	});
    } else {
	writeToLog('<span style="color: brown;">Not a command</span>');
    }
}

function processTrace(trace) {
    // $('#ops').append(parsedTrace);
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
	// var state = item["state"];
	// if (state) {
	//     var node = parms[0]['value'];
	//     var type = parms[0]['type'];
	//     processState(node, type, state);
	//     $('#traceOp' + i).data('state', state);
	// }
    }
    $('div[class="trace"] span[class="var"]').each(function() {
	var from = $(this).text();
	if (typeof varNumericToSymbolicMapping[from] != "undefined") {
	    $(this).text(varNumericToSymbolicMapping[from]);
	}
    });
    if (ignoreChecksums) {
	showElement('.checksum', false);
	$('.checksum').next('.listSeparator').css("display", "none");
    }
}

function findNodeTraceItem(nodeName, startingFrom=0, forward=true) {
    var delta = (forward ? 1 : -1);
    var i = startingFrom + delta;
    while (0 <= i && i < parsedTrace.length) {
	console.log(parsedTrace[i]);
	if (parsedTrace[i]['parameters'][0]['value'] == nodeName) {
	    return parsedTrace[i];
	}
	i += delta;
    }
    return null;
}

function processState(node, type, state) {
    // console.log('Node ' + node);
    // console.log(state);
    if (state['token-store']) {
	updateTokenStoreState(node, type, state['token-store']);
    }
}

function updateTokenStoreState(node, type, newState) {
    if (newState['type'] == 'boolean' && newState['value'] == false) {
	newState = {'type': 'token', 'value': []};
    }
    var prevStates = tokenStores[node];
    if (!prevStates) {
	tokenStores[node] = [ newState ];
    } else {
	console.log('Token store state changed in ' + node + ' from');
	console.log(prevStates[prevStates.length - 1]);
	console.log('to');
	console.log(newState);
	prevStates.push(newState);
    }
}

function showVarPopupMenuDialog(v) {
    // $('#varPopupMenuDialog').dialog( "option", "title", 'Operations on var ' + v);
    // $('#varPopupMenuDialog').html('<ul id="varPopupMenu"><li id="define var">Show nodes defining ' + v + '</li><li id="use var">Show nodes using ' + v + '</li></ul>');
    $('#varMenu').html('<li class="ui-widget-header">Commands</li><li id="define var">Show nodes defining ' + v + '</li><li id="use var">Show nodes using ' + v + '</li><li>Cancel</li>');
    $('#varMenu').menu();
    $('#varMenu').draggable();
    showElement('#varMenu', true);
    // showElement('#varInfo', true);
    // $('#varPopupMenuDialog').dialog("open");
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

function jsonListToHTML(list, open='[', close=']', separator='<span class="listSeparator">, </span>') {
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
    // alert('Make current op ' + n);
    if (currentOp != null) {
	$('#traceOp'+currentOp).removeClass('currentOp');
    }
    currentOp = n;
    var traceItem = parsedTrace[n];
    var operation = traceItem['operation'];
    var id = '#traceOp'+n;
    var elem = $(id);
    elem.addClass('currentOp');
    elem[0].scrollIntoView({behavior: "smooth", block: "end"});
    var cmd = $('#traceOp' + n + ' span[class="cmd"]').html();
    // var operation = $('#traceOp' + n + ' span[class="function"]').html();
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
	var newState = traceItem['state'];
	if (newState) {
	    var prevTraceItem = findNodeTraceItem(node, n, false);
	    var prevState = (prevTraceItem ? prevTraceItem['state'] : null);
	    console.log('State of node ' + node + 'changed from ');
	    console.log(prevState);
	    console.log('to');
	    console.log(newState);
	    var diffs = stateDiff(prevState, newState);
	    if (diffs != null) {
		console.log('Old tokens removed: ');
		var oldTokens = diffs[0];
		for (i in oldTokens) {
		    console.log('  %o', tokenAsCompactString(oldTokens[i]));
		}
		console.log('New tokens added: ');
		var newTokens = diffs[1];
		for (i in newTokens) {
		    console.log('  %o', tokenAsCompactString(newTokens[i]));
		}
	    }
	    nodeState[node] = newState;
	}
    }
}



// function fixToken(x) {
//     if (x['type'] == 'boolean' && x['value'] == false) {
// 	x['type'] = 'token';
// 	x['value'] = [];
//     }
//     return x;
// }

function filterChecksums(values) {
    return values.filter(x => x['type'] != 'checksum');
}

function tokenAsString(token) {
    function convertValues(v) {
	if (v instanceof Array) {
	    return v.map(x => convertValues(x)).join();
	} else if (v instanceof Object) {
	    return '{' + Object.keys(v).map(k => k + ': ' + convertValues(v[k])).join(separator=", ") + '}';
	} else {
	    return v;
	}
	    
    }
    return token['type'] + '(' + convertValues(token['value']) + ')';
}

function tokenAsCompactString(token) {
    function convertValues(v) {
	if (v instanceof Array) {
	    return filterChecksums(v).map(x => convertValues(x)).join();
	} else if (v['type'] == 'binding') {
	    return varNumericToSymbolicMapping[v['var']['value']] + " = " + v['value']['value'];
	} else if (v instanceof Object) {
	    return '{' + Object.keys(v).map(k => k + ': ' + convertValues(v[k])).join(separator=", ") + '}';
	} else {
	    return v;
	}
	    
    }
    return token['type'] + '(' + convertValues(token['value']) + ')';
}

function setDifference(s1, s2) {
    // console.log('setDifference(' + s1 + ',' + s2);
    return s1.filter(function (element, index, array) {
	return !s2.includes(element);
    });
}

function stateDiff(prevState, newState) {
    console.log('stateDiff');
    console.log(prevState);
    console.log(newState);
    if (newState['token-store'] != undefined) {
	var prevTokens = null;
	if (prevState == null) {
	    prevTokens = [];
	} else {
	    prevTokens = prevState['token-store'];
	}
	var newTokens = newState['token-store'];
	return [ setDifference(prevTokens, newTokens), setDifference(newTokens, prevTokens) ];
    } else {
	return null;
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
