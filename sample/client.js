var users = [];

function insertBottom (tag, val, tgt) {
  (function(){ var div = qn(tag); div.innerHTML = val;
  var t = qi(tgt); t.appendChild(div); t.scrollTop = t.scrollHeight; })();
}

function warning(data) { insertBottom('p', data, 'warnings'); ws.close(); }

document.addEventListener("DOMContentLoaded", function(event) { 
    qi('message-form').addEventListener("submit", function(){
        ws.send(enc(tuple(atom('MSG'),bin(qi('text').value))));
	qi('text').value = '';
        return false;
    });

    qi('join-form').addEventListener("submit", function(){
        qi('warnings').innerHTML = '';
        ws.send(enc(tuple(atom('LOGON'),bin(qi('user').value))));
        return false;
    });
});
