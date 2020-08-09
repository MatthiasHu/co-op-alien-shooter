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

  tryConnect();

  draw(ctx);
}

function tryConnect() {
  // const url = "wss://monus.de/co-op-alien-shooter-entry/default"
  const url = "ws://127.0.0.1:58436/default"
  const socket = new WebSocket(url);
  socket.onclose = () => {console.log("socket closed");}
  socket.onerror = () => {console.log("socket error");}
  socket.onopen = () => {console.log("socket connected");}
  socket.onmessage = onMessage;
}

function onMessage(e) {
  // do something with the blob e.data ...
  console.log("received data of size " + e.data.size);
  const reader = new FileReader();
  reader.onload = () => {
    const arr = new Uint16Array(reader.result);
    console.log("first byte: " + arr[0])
    // ...
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

function draw(ctx) {
  drawBackground(ctx);
}

function drawBackground(ctx) {
  ctx.fillStyle = "#0000ff";
  ctx.fillRect(0, 0, 100, 100);
}
