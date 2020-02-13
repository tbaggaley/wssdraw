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

    const sock = new WebSocket("ws://35.246.65.29/ws");

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

                document.getElementById("stage").style.display = "unset";
            }
        });

        log(label, nameInput, submit);
        logScrolling = false;
    };

    sock.onmessage = ({data}) => { 
        const [type, clientID, ...rest] = data.split(",");

        switch(type) {
            case "NEW":
                log("Someone is connecting...");

                clients[clientID] = { 
                    mouse: {
                        down: undefined, x: undefined, y: undefined
                    },
                    brush: {
                        color: "#000000",
                        thickness: 5
                    },
                    name: "new player"
                };
                break;

            case "M_MOV":
                const [mouseX, mouseY] = rest;
                const client = clients[clientID];

                if(client.mouse.down === true) {
                    ctx.lineWidth = client.brush.thickness;
                    ctx.strokeStyle = client.brush.color;
                    ctx.beginPath();
                    ctx.moveTo(client.mouse.x, client.mouse.y);
                    ctx.lineTo(mouseX, mouseY);
                    ctx.stroke();
                }

                clients[clientID].mouse.x = mouseX;
                clients[clientID].mouse.y = mouseY;
                break;

            case "M_DOWN":
                clients[clientID].mouse.down = true;
                break;

            case "M_UP":
                clients[clientID].mouse.down = false;
                break;

            case "COLOR":
                clients[clientID].brush.color = rest;
                break;

            case "SIZE":
                clients[clientID].brush.thickness = rest;
                break;

            case "DISCONNECT":
                log(`${clients[clientID].name} disconnected.`);
                delete clients[clientID];
                break;

            case "NAME":
                const [newName] = rest;
                log(`${newName} has joined the session`);
                clients[clientID].name = newName;
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

    overlay.onmousedown = () => { sock.send("M_DOWN"); };
    overlay.onmouseup = () => { sock.send("M_UP"); };
    overlay.onmousemove = ({offsetX, offsetY}) => { sock.send(`M_MOV,${offsetX},${offsetY}`); };

    document.getElementById("brushColor").onchange = function() { 
        sock.send(`COLOR,${this.value}`);
    };

    document.getElementById("brushSize").onchange = function() {
        sock.send(`SIZE,${this.value}`);
    };

    overlayCtx.font = "18px monospace";
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
