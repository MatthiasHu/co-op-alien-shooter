"use strict";

function onLoad() {
  console.log("ok, let's do this...");

  const canvas = document.getElementById("game-canvas")
  const ctx = canvas.getContext("2d");

  const surface =
    { canvas: canvas
    , ctx: ctx
    , dim: 0
    , center: {x: 0, y: 0}
    };

  adjustSize(surface);

  const socket = tryConnect(surface);

  const inputKeysState = {bits: 0};

  document.addEventListener("keydown",
    function(e) {onKeyDown(socket, inputKeysState, e);} );
  document.addEventListener("keyup",
    function(e) {onKeyUp(socket, inputKeysState, e);} );
  window.addEventListener("resize",
    function(e) {adjustSize(surface);} );
}

function tryConnect(surface) {
  // const url = "wss://monus.de/co-op-alien-shooter-entry/default"
  const url = "ws://127.0.0.1:58436/default"
  const socket = new WebSocket(url);
  socket.onclose = () => {console.log("socket closed");}
  socket.onerror = () => {console.log("socket error");}
  socket.onopen = () => {console.log("socket connected");}
  socket.onmessage = (e) => {onMessage(surface, e);}

  return socket;
}

function onMessage(surface, e) {
  // do something with the blob e.data ...
  const reader = new FileReader();
  reader.onload = () => {
    const dataView = new DataView(reader.result);
    // (DataView uses big endian by default.)
    draw(surface, dataView);
  };
  reader.readAsArrayBuffer(e.data);
}

function adjustSize(surface) {
  const dim = Math.min(window.innerWidth, window.innerHeight) - 10;
  surface.dim = dim;
  surface.canvas.width = dim;
  surface.canvas.height = dim;
  surface.ctx.font = (surface.dim*0.06)+"px DUMMY";
  surface.ctx.textBaseline = "middle";
  surface.ctx.textAlign = "center";
}

function draw(surface, commands) {
  const d = surface.dim;
  const ctx = surface.ctx;

  ctx.fillStyle = "#000000";
  ctx.fillRect(0, 0, d, d);

  const commandLength = 6;

  for (let offset = 0;
       offset + commandLength <= commands.byteLength;
       offset += commandLength) {
    const imageName = commands.getUint16(offset + 0) + ".png";
    const x = ((    (commands.getUint16(offset + 2) / (2**16))) * 2 - 0.5) * d;
    const y = ((1 - (commands.getUint16(offset + 4) / (2**16))) * 2 - 0.5) * d;
    // console.log("should draw " + imageName);
    ctx.fillStyle = "#ff0000";
    ctx.fillRect(x-5, y-5, 10, 10);
  }
}

function onKeyDown(socket, inputKeysState, e) {
  onKey(socket, inputKeysState, e, true);
}
function onKeyUp(socket, inputKeysState, e) {
  onKey(socket, inputKeysState, e, false);
}

function onKey(socket, inputKeysState, e, isDown) {
  const index = keyCodeToSerializationIndex(e.keyCode);

  if (index >= 0) {
    e.preventDefault();

    const bit = 2**index;
    if (isDown) {
      inputKeysState.bits = inputKeysState.bits | bit;
    }
    else {
      inputKeysState.bits = inputKeysState.bits & (~ bit);
    }

    console.log("bits: " + inputKeysState.bits);
    sendInputKeysState(socket, inputKeysState);
  }
}

function keyCodeToSerializationIndex(keyCode) {
  if (keyCode == 37) return 0;
  if (keyCode == 39) return 1;
  if (keyCode == 40) return 2;
  if (keyCode == 38) return 3;
  if (keyCode == 32) return 4;
  return -1;
}

function sendInputKeysState(socket, inputKeysState) {
  const arrayBuffer = new ArrayBuffer(1);
  const view = new DataView(arrayBuffer);
  view.setUint8(0, inputKeysState.bits);
  socket.send(arrayBuffer);
}
