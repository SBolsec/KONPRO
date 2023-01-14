const resultDiv = document.getElementById("remaining-time");

if ("WebSocket" in window) {
    const ws = new WebSocket(`ws://${window.location.host}/remaining-time`);
    let interval;

    ws.onopen = function () {
        console.log("Connection opened");

        ws.send("get");
        console.log("Message sent");

        interval = setInterval(() => {
            ws.send("get");
            console.log("Message sent");
        }, 10000);
    }

    ws.onmessage = function (event) {
        let content = resultDiv.innerHTML;
        content += createRemainingTimeDiv(event.data);
        resultDiv.innerHTML = content;
    }

    ws.onclose = function () {
        console.log("Connection closed");
        clearInterval(interval);
    }
} else {
    console.log("WebSocket not supported");
}

function createRemainingTimeDiv(minutes) {
    if (minutes >= 0) {
        return `<div class="bg-blue-400 text-white p-2 rounded-lg shadow-lg">Show starts in ${minutes} minutes.</div>`;
    } else {
        return `<div class="bg-red-400 text-white p-2 rounded-lg shadow-lg">Show already started ${-minutes} minutes ago.</div>`;
    }
}
