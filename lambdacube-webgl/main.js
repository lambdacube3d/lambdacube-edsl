function $(id) { return document.getElementById(id); }

var gl = null;

var g_canvas = null;
var g_animReqId = null;

var g_pipeline = null;

function draw()
{
  g_pipeline.render();
}

window.onload = function() {
  g_canvas = $('canvas');
  g_canvas.addEventListener('webglcontextlost', contextLost, false);
  g_canvas.addEventListener('webglcontextrestored', contextRestored, false);
  window.onresize();
  g_pipeline = new Pipeline(testIR);
  g_pipeline.allocate(gl);
  animate();
}

window.onresize = function() {
  g_canvas.width = window.innerWidth;
  g_canvas.height = window.innerHeight;
  gl = WebGLUtils.setupWebGL(g_canvas);
  if(!gl) {
    console.log('ERROR: Failed to obtain WebGL context.');
  }
}

function contextLost(e) {
  console.log('contextLost');
  e.preventDefault();
  if(g_animReqId)
  {
    //window.cancelRequestAnimFrame(g_animReqId);
    g_animReqId = null;
  }
}

function contextRestored() {
  console.log('contextRestored');
  animate();
}

function animate() {
  //g_animReqId = window.requestAnimFrame(animate, g_canvas);
  draw();
}
