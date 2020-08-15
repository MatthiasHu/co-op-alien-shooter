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

//  document.addEventListener("keydown",
//    function(e) {onKeyDown(state, e);} );
//  document.addEventListener("keyup",
//    function(e) {onKeyUp(state, e);} );
  window.addEventListener("resize",
    function(e) {adjustSize(surface);} );

  tryConnect(surface);
}

function tryConnect(surface) {
  // const url = "wss://monus.de/co-op-alien-shooter-entry/default"
  const url = "ws://127.0.0.1:58436/default"
  const socket = new WebSocket(url);
  socket.onclose = () => {console.log("socket closed");}
  socket.onerror = () => {console.log("socket error");}
  socket.onopen = () => {console.log("socket connected");}
  socket.onmessage = (e) => onMessage(surface, e);
}

function onMessage(surface, e) {
  // do something with the blob e.data ...
  console.log("received data of size " + e.data.size);
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

  console.log("drawing: " + commands.byteLength + " bytes");
  for (let offset = 0;
       offset + commandLength <= commands.byteLength;
       offset += commandLength) {
    const imageName = commands.getUint16(offset + 0) + ".png";
    const x = (commands.getUint16(offset + 2) / (2**16) * 2 - 0.5) * d;
    const y = (commands.getUint16(offset + 4) / (2**16) * 2 - 0.5) * d;
    console.log("should draw " + imageName);
    ctx.fillStyle = "#ff0000";
    ctx.fillRect(x-5, y-5, 10, 10);
  }
}
