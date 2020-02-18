const canvas = document.getElementById("canvas");
const overlay = document.getElementById("overlay");
const ctx = canvas.getContext("2d");
const overlayCtx = overlay.getContext("2d");

const clients = {};
var myID = '';
var logScrolling = true;

function log() {
    const logElem = document.getElementById("log");

    for(const content of arguments) {
        var appendElem;

        if(content instanceof HTMLElement) 
            appendElem = content;
        else {
            appendElem = document.createElement("p");
            appendElem.innerText = content;
        }

        logElem.appendChild(appendElem);

        if(logScrolling) {
            logElem.scrollTo({top: 200000, behavior: "smooth"});
        }
    }
}

async function sleep(ms) {
    return new Promise((res, rej) => {
        setTimeout(res, ms);
    });
}

function drawCircle(ctx, x, y, radius) {
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI*2);
    ctx.fill();
}

async function init() {
    var playername = "unknown";

    document.getElementById("stage").style.display = "none";

    log("Waking server and receiving IP address...");

    const res = await fetch("https://europe-west1-wssdraw.cloudfunctions.net/start");

    if(res.status !== 200) {
        log("Failed to contact cloud platform. Unable to start instance. Try again later.");
        return
    }

    const ip = await res.text();
    log(`Server alive at ${ip}`);

    while(true) {
        try{
            log("Waiting for websocket server to become ready...");
            const ping = await fetch(`http://${ip}`, {mode: "no-cors"});
            break;
        } catch {
            await sleep(2000);
            continue;
        }
    }
    
    log(`Attempting websocket connection to ${ip}...`);

    const sock = new WebSocket(`ws://${ip}/ws`);

    sock.onopen = () => {
        log("Connected successfully!");

        const label = Object.assign(document.createElement("label"), {
            innerText: "What's your name? "
        });
        const nameInput = document.createElement("input");
        const submit = Object.assign(document.createElement("button"), {
            innerText: "Set name",
            onclick: async () => {
                if(nameInput.value == "")
                    return;

                playername = nameInput.value;
                nameInput.disabled = true;
                submit.disabled = true;

                logScrolling = true;
                sock.send(`NAME,${playername}`);
                sock.send(`COLOR,${document.getElementById("brushColor").value}`);
                sock.send(`SIZE,${document.getElementById("brushSize").value}`);

                document.getElementById("stage").style.display = "unset";
            }
        });

        log(label, nameInput, submit);
        logScrolling = false;
    };

    sock.onmessage = ({data}) => { 
        const [type, clientID, ...rest] = data.split(",");
        const client = clients[clientID];

        switch(type) {
            case "NEW":
                console.log("Someone is connecting...");

                clients[clientID] = { 
                    mouse: {
                        down: undefined, x: undefined, y: undefined
                    },
                    brush: {
                        color: "#000000",
                        thickness: 5
                        alpha: 1.0
                    },
                    name: "new player"
                };
                break;

            case "M_MOV":
                const [mouseX, mouseY] = rest;

                if(client.mouse.down === true) {
                    ctx.strokeStyle = ctx.fillStyle = client.brush.color;
                    ctx.lineWidth = client.brush.thickness*2;
                    ctx.globalAlpha = client.brush.alpha;
                    drawCircle(ctx, mouseX, mouseY, client.brush.thickness);
                    ctx.beginPath();
                    ctx.moveTo(client.mouse.x, client.mouse.y);
                    ctx.lineTo(mouseX, mouseY);
                    ctx.stroke();
                }

                client.mouse.x = mouseX;
                client.mouse.y = mouseY;
                break;

            case "M_DOWN":
                client.mouse.down = true;
                ctx.fillStyle = client.brush.color;
                ctx.globalAlpha = client.brush.alpha;
                drawCircle(ctx, client.mouse.x, client.mouse.y, client.brush.thickness);

                break;

            case "M_UP":
                client.mouse.down = false;
                break;

            case "COLOR":
                client.brush.color = rest;
                break;

            case "ALPHA":
                client.brush.alpha = parseFloat(rest);

            case "SIZE":
                client.brush.thickness = rest;
                break;

            case "DISCONNECT":
                log(`${client.name} disconnected.`);
                delete clients[clientID];
                break;

            case "NAME":
                const [newName] = rest;
                log(`${newName} has joined the session`);
                client.name = newName;
                break;

            case "YOUR_ID":
                myID = clientID;
                break;

            default:
                console.log(`OOB message from server: ${data}`);
        }
    }

    sock.onclose = () => {
        delete clients[myID];
        log("Connection to server lost.");
        log("Will attempt to retry in five seconds...");
        document.getElementById("stage").style.display = "none";
        setTimeout(init, 5000);
    }

    overlay.onpointerdown = () => { sock.send("M_DOWN"); };
    overlay.onpointerup = () => { sock.send("M_UP"); };
    overlay.onpointermove = ({offsetX, offsetY}) => { sock.send(`M_MOV,${offsetX},${offsetY}`); };

    window.addEventListener("keydown", ({key}) => {
        const brushSize = document.getElementById("brushSize");

        switch(key) {
            case "]":
                brushSize.value++;
                break;
            case "[":
                brushSize.value--;
                break;
        }

        brushSize.oninput();
    });

    document.getElementById("brushColor").onchange = function() { 
        sock.send(`COLOR,${this.value}`);
    };

    document.getElementById("brushSize").oninput = function() {
        sock.send(`SIZE,${this.value}`);
    };

    document.getElementById("brushAlpha").oninput = function() {
        sock.send(`ALPHA,${this.value}`);
    }

    document.getElementById("btnDownload").onclick = () =>
    {
        const url = canvas.toDataURL("image/png");
        const a = document.createElement("a");
        a.href = url;
        a.download = "Draw sketch - " + new Date().toUTCString() + ".png";
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
    }

    overlayCtx.font = "18px monospace";

    // Prevent timeout
    setInterval(() => { sock.send("PING"); }, 20000);

    drawLoop();
}

function drawLoop() {
    overlayCtx.clearRect(0, 0, overlay.width, overlay.height);

    for(const [clientID, client] of Object.entries(clients)) {
        overlayCtx.fillText(client.name, parseInt(client.mouse.x) + 10, parseInt(client.mouse.y) - 10);

        overlayCtx.beginPath();
        overlayCtx.arc(client.mouse.x, client.mouse.y, client.brush.thickness, 0, 2*Math.PI);
        overlayCtx.stroke();
    }

    window.requestAnimationFrame(drawLoop);
}

init();
