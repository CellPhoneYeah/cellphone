// 启动时绑定函数
//$(document).ready(getServerInfo);

function getServerInfo()
{
    http = getXMLHttpRequest();
    var url = "getmsg?time=" + Math.random();
    http.onreadystatechange = getInfoBack;
    http.open("GET", url, true);
    http.send(null);
}

function getInfoBack()
{
    if(http.readyState == 4 && http.status == 200)
    {
        var response = http.responseText;
        oldVal = $("#chatbox").val();
        $("#chatbox").val(oldVal + response + "\r\n");
        setTimeout('getServerInfo()', 50000);
    }
}

function sendMsg()
{
    myform.submit();

    $("#msginput").val("");

    var txt = $("#chatbox");
    txt.scrollTop = txt.scrollHeight;
}
    
