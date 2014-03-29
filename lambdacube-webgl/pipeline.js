function Pipeline(desc) {
    this.desc             = desc;
    this.gl               = null;
    this.glPrograms       = null;
    this.glTextures       = null;
    this.glSamplers       = null;
    this.glTargets        = null;
    this.glCommands       = null;
    this.glSlotPrograms   = null;
    this.glInput          = null;
    this.glSlotNames      = null;
    this.glVAO            = null;
    this.glTexUnitMapping = null;
}

Pipeline.prototype.allocate = function(gl) {
    this.gl = gl;
    /* create programs */
    for(var i = 0; i < this.desc.programs.length; ++i) {
        this.glPrograms = this.createProgram(this.desc.programs[i]);
    }
    this.glCommands = this.desc.commands;
}

Pipeline.prototype.createProgram = function(program) {
    var prog = gl.createProgram();
    var vs = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vs, program.vertexShader);
    gl.compileShader(vs);
    gl.attachShader(prog, vs);
    console.log('vs info: ' + gl.getShaderInfoLog(vs));
    var fs = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fs, program.fragmentShader);
    gl.compileShader(fs);
    gl.attachShader(prog, fs);
    console.log('fs info: ' + gl.getShaderInfoLog(fs));
    gl.linkProgram(prog);
    console.log('prog info: ' + gl.getProgramInfoLog(prog));
    return prog;
}

Pipeline.prototype.dispose = function() {

}

Pipeline.prototype.bindInput = function(input) {

}

Pipeline.prototype.render = function() {
    for(var i = 0; i < this.glCommands.length; ++i) {
        var command = this.glCommands[i];
        this[command.tag](command.contents);
    }
}

/* COMMANDS */

Pipeline.prototype.SetRasterContext = function(args) {
    console.log('TODO: implement SetRasterContext');
}

Pipeline.prototype.SetAccumulationContext = function(args) {
    console.log('TODO: implement SetAccumulationContext');
}

Pipeline.prototype.SetRenderTarget = function(index) {
    this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, null);
}

Pipeline.prototype.SetProgram = function(index) {
    this.gl.useProgram(this.glPrograms[index]);
}

Pipeline.prototype.SetSamplerUniform = function(args) {
    console.log('TODO: implement SetSamplerUniform');
}

Pipeline.prototype.SetTexture = function(args) {
    console.log('TODO: implement SetTexture');
}

Pipeline.prototype.SetSampler = function(args) {
    console.log('TODO: implement SetSampler');
}

Pipeline.prototype.RenderSlot = function(args) {
    console.log('TODO: implement RenderSlot');
}

Pipeline.prototype.ClearRenderTarget = function(pairs) {
    var mask = 0;
    for(var i = 0; i < pairs.length; ++i) {
        var pair = pairs[i];
        var lut = {
            'Color': function(value) {
                mask |= gl.COLOR_BUFFER_BIT;
                if (value.tag == 'VV4F') {
                    var color = value.contents;
                    gl.clearColor(color[0], color[1], color[2], color[3]);
                } else {
                    console.log('TODO: implement ClearRenderTarget for formats other than VV4F');
                    gl.clearColor(0, 0, 1, 1);
                }
            },
            'Depth': function() {
                mask |= gl.DEPTH_BUFFER_BIT;
                console.log('TODO: implement ClearRenderTarget for Depth value');
            },
            'Stencil': function() {
                mask |= gl.STENCIL_BUFFER_BIT;
                console.log('TODO: implement ClearRenderTarget for Stencil value');
            }
        };
        lut[pair[0]](pair[1]);
    }
    gl.clear(mask);
}

Pipeline.prototype.GenerateMipMap = function(args) {
    console.log('TODO: implement GenerateMipMap');
}

Pipeline.prototype.SaveImage = function(args) {
    console.log('TODO: implement SaveImage');
}

Pipeline.prototype.LoadImage = function(args) {
    console.log('TODO: implement LoadImage');
}

/* INPUTS */

function PipelineInput(schema) {
}

PipelineInput.prototype.allocate = function(gl) {
    this.gl = gl;
}

PipelineInput.prototype.dispose = function() {

}
