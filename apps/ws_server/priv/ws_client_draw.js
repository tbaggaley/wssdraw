const canvas = document.getElementById("canvas");
const overlay = document.getElementById("overlay");
const ctx = canvas.getContext("2d");
const overlayCtx = overlay.getContext("2d");

const clients = {};

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
        logElem.scrollTo({top: 200000, behavior: "smooth"});
    }
}

var g_messageRef = 0;
async function server_call(websocket, text) {
    const requestRef = g_messageRef++;
    websocket.send(`${requestRef},${text}`);
    return new Promise((resolve, reject) => {
        // Give the reply up to 1s, otherwise reject promise (server dead?)
        const rejectTimer = setTimeout(reject, 1000);

        websocket.addEventListener("message", ({data}) => {
            [replyRef, ...rest] = data.split(",");
            if(replyRef == requestRef) {
                clearTimeout(rejectTimer);
                resolve(rest.join(","));
            }
        });
    });
}

function init() {
    var playername = "unknown";

    document.getElementById("stage").style.display = "none";

    log("Attempting to connect to server...");
    
    const sock = new WebSocket("wss://wssrepl.tbagga.repl.co/ws");
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

                log(`Hi, ${playername}. Registering name with server...`);
                await server_call(sock, `HELLO,${playername}`);
                log("Registered successfully.");

                document.getElementById("stage").style.display = "unset";
            }
        });

        log(label, nameInput, submit);
    };

    sock.onmessage = ({data}) => { 
        const [type, clientID, ...rest] = data.split(",");

        switch(type) {
            case "NEW":
                console.log(`New client connected: ${clientID}`);

                clients[clientID] = { 
                    mouse: {
                        down: undefined, x: undefined, y: undefined
                    },
                    brush: {
                        color: "#000000",
                        thickness: 5
                    }
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

            case "DISCONNECT":
                log(`${clientID.name} disconnected.`);
                delete clients[clientID];
                break;

            default:
                console.log(`OOB message from server: ${data}`);
        }

    sock.onclose = () => {
        log("Connection to server lost.");
        log("Will attempt to retry in five seconds...");
        document.getElementById("stage").style.display = "none";
        setTimeout(init, 5000);
    }

    overlay.onmousedown = () => { sock.send("M_DOWN"); };
    overlay.onmouseup = () => { sock.send("M_UP"); };
    overlay.onmousemove = ({offsetX, offsetY}) => { sock.send(`M_MOV,${offsetX},${offsetY}`); };

    drawLoop();
}

function drawLoop() {
    setTimeout(drawLoop, 40);
}

init();
