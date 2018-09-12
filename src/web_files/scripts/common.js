function getXMLHttpRequest()
{
    var xmlHttp= null;
    try
    {
        xmlHttp = new XMLHttpRequest();
    }
    catch (e)
    {
        console.log(e);
        try
        {
            xmlHttp = new ActiveXObject("Msxml2.XMLHTTP");
        }
        catch (e)
        {
            console.log(e);
            xmlHttp = new ActiveXObject("Micarosoft.XMLHTTP");
        }
    }
    return xmlHttp;
}
