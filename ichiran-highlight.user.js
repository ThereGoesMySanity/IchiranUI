// ==UserScript==
// @name     ichiran-highlight
// @version  1
// @include  *
// @grant    GM.xmlHttpRequest
// @grant    GM_xmlHttpRequest
// ==/UserScript==

(() => {
    var server = "localhost";
    var port = "13536";

    function getSelectionText() {
        var text = "";
        var activeEl = document.activeElement;
        var activeElTagName = activeEl ? activeEl.tagName.toLowerCase() : null;
        if (
            (activeElTagName == "textarea") || (activeElTagName == "input" &&
                /^(?:text|search|password|tel|url)$/i.test(activeEl.type)) &&
            (typeof activeEl.selectionStart == "number")
        ) {
            text = activeEl.value.slice(activeEl.selectionStart, activeEl.selectionEnd);
        } else if (window.getSelection) {
            text = window.getSelection().toString();
        }
        return text;
    }
    // backwards compatible
    var GMX;
    if(typeof GM == "undefined"){
        GMX = {
            xmlHttpRequest: GM_xmlhttpRequest,
            setValue: function(name, value){
                return Promise.resolve(GM_setValue(name, value));
            },
            getValue: function(name, def){
                return Promise.resolve(GM_getValue(name, def));
            }
        };
    }else{
        GMX = GM;
    }
    document.addEventListener('keydown', function(event) {
        if (event.altKey && event.key === 'o') {
            var text = getSelectionText();
            GMX.xmlHttpRequest({
                method: "POST",
                url: `http://${server}:${port}/`,
                headers: {
                    "Content-Type": "text/plain"
                },
                data: text
                //onload:
            });
        }
    });

})();
