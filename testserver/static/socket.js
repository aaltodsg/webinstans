var messages = null;

function init()
{
    messages = document.getElementById("messages");
    testWebSocket();
}

function testWebSocket()
{
    websocket = new WebSocket(wsUri);
    websocket.onopen = function(evt) { onOpen(evt) };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };


function onOpen(evt)
{
    msg('<span class="message">CONNECTED</span>');
    doSend("WebSocket rocks");
}

function onClose(evt)
{
    msg('<span class="message">DISCONNECTED</span>');
}

function onMessage(evt)
{
    msg('<span class="message" style="color: blue;">RESPONSE: ' + evt.data+'</span>');
    websocket.close();
}

function onError(evt)
{
    msg('<span class="message" style="color: red;">ERROR:</span> ' + evt.data);
}

function doSend(message)
{
    msg('<span class="message">SENT: ' + message + '</span>'); 
    websocket.send(message);
}

function msg(message)
{
    var pre = document.createElement("p");
    pre.style.wordWrap = "break-word";
    pre.innerHTML = message;
    messages.appendChild(pre);
}

window.addEventListener("load", init, false);
