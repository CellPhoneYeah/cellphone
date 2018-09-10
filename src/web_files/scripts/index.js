var socket;
function ConnectServer()
{
    try 
    {
        socket  = new WebSocket("ws://192.168.1.167:8080");
    }
    catch(e)
    {
        alert("error" + e);
        return;
    }
    socket.onopen = sOpen;
    socket.onerror = sError;
    socket.onmessage = sMessage;
    socket.onclose = sClose;
}

function sOpen()
{
    alert('connect success!');
}

function sError(e)
{
    alert("error" + e);
}

function sMessage(msg)
{
    alert("server says:" + msg);
}

function sClose(e)
{
    alert("connect closed:" + e.code);
}

function Send()
{
    socket.send(document.getElementById("sendContent").value);
}

function Close()
{
    socket.close();
}
