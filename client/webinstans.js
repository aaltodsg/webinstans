//  var wsUri = "ws://echo.websocket.org/";
var wsUri = "ws://localhost:12345/bongo";
var log;
var instansResults = {};
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
    $('#tabs').tabs();
    log = document.getElementById("log");
    // $(document).click(function (e) {
    // 	console.log('click %o', e);
    // });
    
    // $(function() {
    // 	$( "#runContainer" ).accordion({
    // 	    collapsible: true,
    // 	    active: false
    // 	});
    // }); 
    // $(function() {
    // 	$( "#logContainer" ).accordion({
    // 	    collapsible: true,
    // 	    active: false
    // 	});
    // }); 
    // $(function() {
    // 	$( "#varContainer" ).accordion({
    // 	    collapsible: true,
    // 	    active: false
    // 	});
    // }); 
    initWebSocket();
    $('#rewindButton').click(function() {
	moveToOp(0);
    });
    $('#stepBackButton').click(function() {
	if (currentOp) {
	    moveToOp(currentOp - 1);
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
	    moveToOp(currentOp + 1);
	}
    });
    $('#endButton').click(function() {
	moveToOp($('#ops div').length - 1);
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


function handleInstansResults() {
    console.log('set graph');
    $('#graph').html(instansResults.dotResultGraph);
    showElement('#graph', true);
    console.log('parse var mappings');
    console.log(instansResults.varMappings);
    var varMappings = jQuery.parseJSON(instansResults.varMappings);
    for (var i in varMappings) {
	var mapping = varMappings[i];
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
    console.log('parse defining nodes');
    var dn = jQuery.parseJSON(instansResults.definingNodes);
    for (var i in dn) {
	var item = dn[i];
	var v = item[0];
	var nodes = item[1];
	definingNodes[v] = nodes;
	// alert(v + ' -> ' + nodes.length + ' nodes');
    }
    console.log('parse using nodes');
    var un = jQuery.parseJSON(instansResults.usingNodes);
    for (var i in un) {
	var item = un[i];
	var v = item[0];
	var nodes = item[1];
	usingNodes[v] = nodes;
	// alert(v + ' -> ' + nodes.length + ' nodes');
    }
    console.log('parsing trace');
    console.log(instansResults.trace);
    parsedTrace = jQuery.parseJSON(instansResults.trace);
    console.log(parsedTrace);
    showElement('#ops', true);
    processTrace(parsedTrace);
    showElement('#player', true);
    console.log('parsing end');
    var status = stringBefore(instansResults.end, ' ');
    var rest = stringAfter(instansResults.end, ' ');
    $('#executionInfo').text('Execution ' + instansResults.end + '. ' + parsedTrace.length + ' operations');
    if (status == "failed") {
	$('#executionInfo').addClass('executionFailed');
    }
    moveToOp(0);
    // showEdgeTraverseInfo();
    // for (var k = 0 ; k <= trackCounter; k++) {
    //     highlightTrackEnterOperations(k);
    // }
    // $('.var').click(function() {
    // 	showVarPopupMenuDialog($(this).text());
    // });
    // addNodeButtonHandlers();
    $('#tabs').tabs('refresh');
    return true;
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
    function handleCmd(expected, key) {
	if (cmd == expected) {
	    writeToLog('<span style="color: blue;">' + expected + '</span>');
            instansResults[key] = args;
	    return true;
	} else return false;
    }
    handleCmd("dot-result", 'dotResultGraph')
	|| handleCmd("var-mappings", 'varMappings')
	|| handleCmd("defining-nodes", 'definingNodes')
	|| handleCmd("using-nodes", 'usingNodes')
	|| handleCmd("trace", 'trace')
	|| handleCmd("end", 'end') && handleInstansResults()
	|| writeToLog('<span style="color: brown;">Not a command</span>');
}

function processTrace(trace) {
    // $('#ops').append(parsedTrace);
    for (var i in trace) {
	let j = i;
	var item = trace[j];
	console.log('process trace item %o', item);
	var node = item["node"];
	var direction = item["direction"];
	var op = item["function"];
	var parms = item["parameters"];
	var state = item["state"];
	var delta = item["delta"];
    	var indent = null;
    	if (direction == "enter") {
    	    indent = new Array((traceLevel+1)*traceLevelIndent).join('&nbsp;')
    	    if (traceLevel == 0) {
    		trackCounter = trackCounter + 1;
    		trackEnterOps.push([]);
    		console.log('>>>Increment trackCounter to ' + trackCounter + ' at operation ' + j);
    		console.log('>>>trackEnterOps[' + (trackCounter-1) + ']' + trackEnterOps[trackCounter-1]);
    	    }
    	    traceLevel = traceLevel + 1;
    	    trackEnterOps[(trackCounter-1)].push(j);
    	} else {
    	    traceLevel = traceLevel - 1;
    	    indent = new Array((traceLevel+1)*traceLevelIndent).join('&nbsp;')
    	}
	var callHTML = callToHTML(j, node, direction, op, parms);
	var stateHTML = stateToHTML(i, node, state, delta);
    	$('#ops').append('<div id="traceOp' + j + '"class="trace"></div>').find('div:last-child').append(callHTML).append('<span>&nbsp;&nbsp;</span>').append(stateHTML).prepend(indent);
	$('#traceOp' + j).data('node', node);
	$('#traceOp' + j + ' .call').data('index', j);
	$('#traceOp' + j + ' .state').data('state', state);
	$('#traceOp' + j + ' .state').data('delta', delta);
    }
    $('.call').click(function (e) {
	var index = $(this).data('index');
	console.log('click call %d, $(this) = %o, index = %o', e, index, $(this));
    	moveToOp(index);
    });
    $('.state .count').click(function (e) {
	var parent = $(this).parent();
	var key = parent.attr('key');
	var state = parent.data('state');
	console.log('click state key = %o, state = %o\n%o', key, state, state[key]);
	openStateDialog($(this));
    });
    $('.state.changed .plus').click(function (e) {
	var parent = $(this).parent();
	var key = 'added-' + parent.attr('key');
	var delta = parent.data('delta');
	console.log('click delta key = %o, delta = %o\n%o', key, delta, delta[key]);
    });
    $('.state.changed .minus').click(function (e) {
	var parent = $(this).parent();
	var key = 'removed-' + parent.attr('key');
	var delta = parent.data('delta');
	console.log('click delta key = %o, delta = %o\n%o', key, delta, delta[key]);
    });
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

function openStateDialog(jq) {
    var state = jq.parent();
    var tr = state.parent();
    var node = tr.data('node')['value'];
    var key = state.attr('key');
    var dialog_id = node + '-' + key;
    var title = node + ' ' + key;
    // console.log('openStateDialog jq %o parent %o node %o key %o, dialog_id %o', jq, jq.parent(), node, key, dialog_id);
    if ($('#' + dialog_id).size() == 0) {
	jq.append('<div id="' + dialog_id + '" title="' + title + '"><div>&nbsp;</div></div>');
	$('#' + dialog_id).dialog({width: 40, height: 30});
	$('#' + dialog_id).dialog('widget').position({
	    // my: 'left top',
	    // at: 'left bottom',
	    my: 'center top',
	    at: 'center bottom',
	    of: jq
	});
    } else {
	$('#' + dialog_id).dialog('open');
    }
    var state = state.data('state');
    console.log('#%s = %o', dialog_id, $('#' + dialog_id));
    $('#' + dialog_id).each(function () {
	$(this).html('');
	var list = state[key];
	console.log('adding items %o to dialog', list);
	for (i in list) {
	    var it = list[i];
	    console.log('adding %o to %o', it, $(this));
	    var itString;
	    if (key == 'tokens' || key == 'solutions') {
		itString = tokenAsCompactString(it);
	    } else if (key == 'beta-items' || key == 'alpha-items') {
		itString = it[0].map(function (x) { return x['value']}).join(separator=", ") + ' -> ' + tokenAsCompactString(it[1]);
	    } else {
		itString = '???';
	    }
	    $(this).append(div('stateLine', span('stateLineContent', itString)));
	}
    });
}

//     $('#' + id).each(function () {
// 	var dialog_id = id + '_tokens';
// 	if (!document.getElementById(dialog_id)) {
// 	    $('body').append('<div id="' + dialog_id + '" title="' + id + '"><div>&nbsp;</div></div>');
// 	    $('#' + dialog_id).dialog({width: 40, height: 30, autoOpen: false});
// 	    $('#' + dialog_id).attr('class', 'token-store');
// 	    $(this).click(function(e) {
// 		$('#' + dialog_id).dialog('open');
// 		// var pos = $(e.delegateTarget).position();
// 		// var x = pos.x;
// 		// var y = pos.y;
// 		// var width = e.delegateTarget.offsetWidth;
// 		// var height = e.delegateTarget.offsetHeight;
// 		// console.log('click on ' + id + ', target = %o [%o], pos=%o, [%o, %o]', e.delegateTarget, typeof(e.delegateTarget), pos, width, height);
// 		// $('#' + dialog_id).css({position: 'absolute', top: y, left: x});
// 		$('#' + dialog_id).dialog('widget').position({
// 		    // my: 'left top',
// 		    // at: 'left bottom',
// 		    my: 'center top',
// 		    at: 'center bottom',
// 		    of: e.delegateTarget
// 		});
// 	    });
// 	}
//     });
// }

// function addNodeButtonHandlers() {
//     console.log('----> addNodeButtonHandlers');
//     $('g[class="node"]').each(function () {
//     	console.log('Testing %o', $(this));
//     	var id = $(this).attr('id');
//     	if (id && id.match(/^(beta|alpha)_memory\d+$/)) {
//     	    console.log('Found %o', id);
// 	    createStoreDialog(id);
//     	}
//     });
//     console.log('<---- addNodeButtonHandlers');
// }

function findNodeTraceItem(nodeName, startingFrom, forward) {
    var delta = (forward ? 1 : -1);
    var i = startingFrom + delta;
    while (0 <= i && i < parsedTrace.length) {
	// console.log(parsedTrace[i]);
	if (parsedTrace[i]['parameters'][0]['value'] == nodeName) {
	    return parsedTrace[i];
	}
	i += delta;
    }
    return null;
}

// function showVarPopupMenuDialog(v) {
//     // $('#varPopupMenuDialog').dialog( "option", "title", 'Operations on var ' + v);
//     // $('#varPopupMenuDialog').html('<ul id="varPopupMenu"><li id="define var">Show nodes defining ' + v + '</li><li id="use var">Show nodes using ' + v + '</li></ul>');
//     $('#varMenu').html('<li class="ui-widget-header">Commands</li><li id="define var">Show nodes defining ' + v + '</li><li id="use var">Show nodes using ' + v + '</li><li>Cancel</li>');
//     $('#varMenu').menu();
//     $('#varMenu').draggable();
//     showElement('#varMenu', true);
//     // showElement('#varInfo', true);
//     // $('#varPopupMenuDialog').dialog("open");
// }

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
	var elem = jQuery('<span>'+jsonListToHTML(jsonParams[1]["value"], '[', ']')+'</span>');
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

function div(cls, content) {
    return '<div class="' + cls + '">' + content + '</div>';
}

function callToHTML(i, node, direction, operation, jsonParams) {
    // alert(jsonParams);
    return span('call', span("direction", direction) + '&nbsp;' + span("function", operation) + '&nbsp;' + jsonListToHTML(jsonParams, '(', ')'));
}

function stateToHTML(i, node, state, delta) {
    console.log('node %o, state %o, delta %o', node, state, delta);
    function stateDescr(key) {
	var count = state[key].length;
	var removed = (delta ? delta['removed-' + key].length : 0);
	var added = (delta ? delta['added-' + key].length : 0);
	var content;
	var cls;
	if (removed != 0 || added != 0) {
	    cls = 'state changed';
	    content = span('count changed', count + ' '+ key) + (removed ? span('minus', removed) : '') + (added ? span('plus', added) : '');
	} else {
	    cls = 'state';
	    content = span("count", count + ' '+ key);
	}
	return '<span class="' + cls + '" key="' + key + '">' + content + '</span>';
    }
    if (state) {
	switch (state['type']) {
	case 'token-store-state':
	    return stateDescr('tokens', 'state');
	    break;
	case 'join-node-state':
	    return stateDescr('beta-items', 'stateBeta') + '<span>&nbsp;&vert;&nbsp;</span>' + stateDescr('alpha-items', 'stateAlpha');
	case 'existence-start-node-state':
	    return '';
	    var aux = countAndChanges('tokens');
	    return stateDescr(aux.content, aux.spanClass);
	    break;
	case 'query-node-state':
	    return '';
	    var aux = countAndChanges('solutions');
	    return stateDescr(aux.content, aux.spanClass);
	    break;
	}
    } else {
	return '';
    }
}

// function stateToHTML(i, node, state, delta) {
//     console.log('node %o, state %o, delta %o', node, state, delta);
//     function countAndChanges(key) {
// 	var count = state[key].length;
// 	var removed = (delta ? delta['removed-' + key].length : 0);
// 	var added = (delta ? delta['added-' + key].length : 0);
// 	if (removed != 0 || added != 0) {
// 	    return { content: span('countChanged', count + ' '+ key) + (removed ? span('countMinus', removed) : '') + (added ? span('countPlus', added) : ''),
// 		     spanClass:  'stateSummaryChanged'}
// 	} else {
// 	    return { content: span("count", count + ' '+ key),
// 		     spanClass: 'stateSummary'};
// 	}
//     }
//     function stateDescr(content, spanClass) {
// 	return '<span class="' + spanClass + '" id="state' + i + '">' + content + '</span>';
//     }
//     if (state) {
// 	switch (state['type']) {
// 	case 'token-store-state':
// 	    var aux = countAndChanges('tokens');
// 	    return stateDescr(aux.content, aux.spanClass);
// 	    break;
// 	case 'join-node-state':
// 	    var beta = countAndChanges('beta-items');
// 	    var alpha = countAndChanges('alpha-items');
// 	    return stateDescr(beta.content + '&nbsp;&vert;&nbsp;' + alpha.content,
// 			      (beta.spanClass == 'stateSummary' && beta.spanClass == 'stateSummary' ? 'stateSummary' : 'stateSummaryChanged'));
// 	case 'existence-start-node-state':
// 	    var aux = countAndChanges('tokens');
// 	    return stateDescr(aux.content, aux.spanClass);
// 	    break;
// 	case 'query-node-state':
// 	    var aux = countAndChanges('solutions');
// 	    return stateDescr(aux.content, aux.spanClass);
// 	    break;
// 	}
//     } else {
// 	return '';
//     }
// }

function jsonListToHTML(list, open, close) {
    var separator='<span class="listSeparator">, </span>';
    var converted = []
    console.log('jsonListToHTML %o', list);
    for (var i in list) {
	var o = list[i];
	converted.push(jsonToHTML(o));
    }
    var result = '<span class="listOpen">' + open + '</span>' + converted.join(separator) + '<span class="listClose">' + close + '</span>';
    // alert(result);
    console.log('jsonListToHTML %o -> %o', list, result);
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
	return jsonListToHTML(o, '[', ']');
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
	    return jsonListToHTML(value, '[', ']');
	default:
	    return span(type, value);
	}
    }
}

function nodeHighlightSelector(node) {
    return $('#' + node + ' ellipse');
}

function moveToOp(n) {
    if (n < currentOp) {
	makeCurrentOp(0);
    }
    for (var i = currentOp + 1; i <= n; i++) {
	makeCurrentOp(i);
    }
}

function makeCurrentOp(n) {
    console.log('>>>> Make current op %d', n);
    if (currentOp != null) {
	$('#traceOp'+currentOp).removeClass('currentOp');
    }
    currentOp = n;
    var traceItem = parsedTrace[n];
    var operation = traceItem['function'];
    var id = '#traceOp'+n;
    var elem = $(id);
    elem.addClass('currentOp');
    elem[0].scrollIntoView({behavior: "smooth", block: "end"});
    var direction = $('#traceOp' + n + ' span[class="direction"]').html();
    // var operation = $('#traceOp' + n + ' span[class="function"]').html();
    // alert(operation);
    // console.log('operation %s', operation)
    if (operation == "add-token" || operation == "add-alpha-token" || operation == "add-beta-token" ||
	operation == "remove-token" || operation == "remove-alpha-token" || operation == "remove-beta-token" ||
	operation == "token-store-put" || operation == "token-store-put-if-missing" || 
	operation == "token-store-remove" || operation == "token-store-remove-if-exists" || operation == "token-store-clear" ||
        operation == "index-put-token" || operation == "index-remove-token" ||
        operation == "token-map-put" || operation == "token-map-remove") {
	var node = $('#traceOp' + n + ' span[class="node"]').text().trim();
	console.log('operation "%s" in node "%s"', operation, node);
	if (currentNode && savedCss[currentNode]) {
	    console.log('currentNode was "%s", savedCss = <<<%s>>>', currentNode, savedCss[currentNode]);
	    nodeHighlightSelector(currentNode).css(savedCss[currentNode]);
	}
	currentNode = node;
	savedCss[currentNode] = nodeHighlightSelector(currentNode).css(nodePropNames);
	nodeHighlightSelector(currentNode).css(currentNodeCss(direction, operation));
	var newState = traceItem['state'];
	if (newState) {
	    // createStoreDialog(node);
	    var prevTraceItem = findNodeTraceItem(node, n, false);
	    var prevState = (prevTraceItem ? prevTraceItem['state'] : {});
	    processStateChange(node, prevState, newState);
	}
    }
    // console.log('node "%s", elem.class = "%s"', node, elem.attr('class'));
}

function filterChecksums(values) {
    return values.filter(x => x['type'] != 'checksum');
}

function processStateChange(node, prevState, newState) {
    console.log('State of node "%o" changed from "%o" to "%o"', node, prevState, newState);
    switch (newState['type']) {
    case 'token-store-state':
	updateTokenStoreState(node, prevState, newState);
	break;
    case 'join-node-state':
	updateJoinNodeState(node, prevState, newState);
	break;
    case 'existence-start-node-state':
	updateExistenceStartNodeState(node, prevState, newState);
	break;
    case 'query-node-state':
	updateQueryNodeState(node, prevState, newState);
	break;
    }
    nodeState[node] = newState;
}

function updateTokenStoreState(node, prevState, newState) {
    // {"type": "token-store-state", "tokens": [{ "type": "token", "value": [{ "type": "checksum", "value": 268959755}]}]}
    var prevTokens = prevState['tokens'] || [];
    var newTokens = newState['tokens'] || [];
    console.log('updateTokenStoreState prev = %o, new = %o', prevTokens, newTokens);
    $('#'+node+' text:contains("tokens")').html(newTokens.length + ' tokens');
    console.log('%o has %d tokens', node, newTokens.length);
}

function updateJoinNodeState(node, prevState, newState) {
    // {"type": "join-node-state", "alpha-items": { "type": "boolean", "value": false}, "beta-items": { "type": "boolean", "value": false}}
    updateIndexState(node, 'alpha', prevState['alpha-items'] || [], newState['alpha-items'] || []);
    updateIndexState(node, 'beta', prevState['beta-items'] || [], newState['beta-items'] || []);
}

function updateIndexState(node, kind, prevKeyTokenPairs, newKeyTokenPairs) {
    // [[{ "type": "boolean", "value": false},
    //   [{ "type": "boolean", "value": false}, { "type": "token", "value": [{ "type": "checksum", "value": 268959755}]}, { "type": "token", "value": [{ "type": "checksum", "value": 268959755}]}]]]
    // [[[{ "type": "iri", "value": "<http://www.aalto.fi/adolf>"}],
    //   [{ "type": "boolean", "value": false}, { "type": "token", "value": [{ "type": "checksum", "value": 613030950830435020}, { "type": "binding", "var": { "type": "var", "value": "?2"}, "value": { "type": "integer", "value": 0}}, { "type": "checksum", "value": 613030950830435020}, { "type": "binding", "var": { "type": "var", "value": "?0"}, "value": { "type": "iri", "value": "<http://www.aalto.fi/adolf>"}}]}]]]
    console.log('updateIndexState kind = %s prev = %o, new = %o', kind, prevKeyTokenPairs, newKeyTokenPairs);
    var access = (kind == 'alpha' ? 'last' : 'first')
    var elems = $('#'+node+' text:contains("items"):'+access);
    var oldText = $(elems).text();
    var newText = oldText.replace(/no|\d+/, '' + newKeyTokenPairs.length);
    console.log('oldText = %o, newText = %o', oldText, newText);
    $(elems).text(newText);
}

function updateExistenceStartNodeState(node, prevState, newState) {
    // {"type": "existence-start-node-state", "tokens": { "type": "boolean", "value": false}, "map-items": { "type": "boolean", "value": false}}
    updateTokenMapState(node, prevState['map-items'] || [], newState['map-items'] || []);
}

function updateTokenMapState(node, prevItems, newItems) {
    console.log('updateTokenMapState prev = %o, new = %o', prevItems, newItems);
    $('#'+node+' text:contains("tokens")').html(newItems.length + ' tokens');
    console.log('%o has %d tokens', node, newItems.length);
}

function updateQueryNodeState(node, prevState, newState) {
    // {"type": "query-node-state",
    //  "solutions": [{ "type": "token", "value": [{ "type": "checksum", "value": 613030950830435020}, { "type": "binding", "var": { "type": "var", "value": "?2"}, "value": { "type": "integer", "value": 0}}, { "type": "checksum", "value": 613030950830435020}, { "type": "binding", "var": { "type": "var", "value": "?0"}, "value": { "type": "iri", "value": "<http://www.aalto.fi/adolf>"}}]}]}
    var prevSolutions = prevState['solutions'] || [];
    var newSolutions = newState['solutions'] || [];
    console.log('updateTokenStoreState prev = %o, new = %o', prevSolutions, newSolutions);
    $('#'+node+' text:contains("solutions")').html(newSolutions.length + ' solutions');
    console.log('%o has %d solutions', node, newSolutions.length);
}

function setDifference(s1, s2) {
    console.log('setDifference(s1=%o, s2=%o)', s1, s2);
    return s1.filter(function (element, index, array) {
	return !s2.includes(element);
    });
}

function tokenAsCompactString(token) {
    console.log('tokenAsCompactString %o', token);
    function convertValues(v) {
	console.log('convertValues %o', v);
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
