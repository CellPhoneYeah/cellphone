function run()
{
    var myform = document.getElementById("register_form");
    myform.onsubmit = function(){return checkRegister();};
    console.log($("#register_form").onsubmit);
}


function checkRegister()
{
    var flag = true;
    var name = $("#role_name").val();
    var name_length = name.length;
    if(name_length <= 0 || name == null)
    {
        alert("名字不能为空");
        $("#role_name").focus();
        return false;
    }
    else if(name_length > 5 && name_length < 2)
    {
        alert("名字为2到5个字符,当前长度为" + name_length);
        $("#role_name").focus();
        return false;
    }
    var password = $("#role_password").val();
    var password_length = password.length;
    if(password_length <= 0)
    {
        alert("密码不能为空");
        $("#role_password").focus();
        return false;
    }else if(password_length > 12 || password_length < 6)
    {
        alert("密码为6到12个字符,当前长度为" + password_length);
        $("#role_password").focus();
        return false;
    }
    return true;
}

function roleRegister()
{
    var xmlHttp = getXMLHttpRequest();
    var name = $("#role_name").val();
    var Password = $("#role_password").val();
    var url = "register?name=" + name + "?password=" + Password;
    xmlHttp.open("GET", url, true);
    xmlHttp.send(null);
}
$(document).ready(run);
