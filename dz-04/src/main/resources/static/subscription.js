const resultDiv = document.getElementById("remaining-time");

async function startGet() {
    try {
        const response = await fetch("/remaining-time");

        if (response.ok) {
            const data = await response.json();

            let content = resultDiv.innerHTML;
            content += createRemainingTimeDiv(data.minutes);
            resultDiv.innerHTML = content;
        }
    } catch (err) {
        console.log("timeout", err);
    }
    await startGet();
}

function createRemainingTimeDiv(minutes) {
    if (minutes >= 0) {
        return `<div class="bg-blue-400 text-white p-2 rounded-lg shadow-lg">Show starts in ${minutes} minutes.</div>`;
    } else {
        return `<div class="bg-red-400 text-white p-2 rounded-lg shadow-lg">Show already started ${-minutes} minutes ago.</div>`;
    }
}

startGet();
