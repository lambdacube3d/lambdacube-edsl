function Pipeline(desc) {
    this.desc             = desc;   // Pipeline
    this.gl               = null;   // webgl backend
    this.glPrograms       = [];     // Vector GLProgram
    this.glTextures       = [];     // Vector GLTexture
    this.glSamplers       = [];     // Vector GLSampler
    this.glTargets        = [];     // Vector GLRenderTarget
    this.glCommands       = [];     // [GLCommand]
    this.glSlotPrograms   = [];     // Vector [ProgramName] -- programs depend on a slot
    this.glInput          = null;   // IORef (Maybe InputConnection)
    this.glSlotNames      = [];     // Vector ByteString
    this.glVAO            = null;   // GLuint
    this.glTexUnitMapping = {};     // Trie (IORef GLint)   -- maps texture uniforms to texture units
}

Pipeline.prototype.allocate = function(gl) {
    this.gl = gl;
    /* create programs */
    for(var i = 0; i < this.desc.programs.length; ++i) {
        this.glPrograms.push(this.createProgram(this.desc.programs[i]));
    }
    this.glCommands = this.desc.commands;
}

function GLProgram() {
    this.shaderObjects = [];        // [GLuint]
    this.programObject = null;      // GLuint
    this.inputUniforms = {};        // Trie GLint
    this.inputTextures = {};        // Trie GLint   -- all input textures (render texture + uniform texture)
    this.inputTextureUniforms = {}; // Set UniformName
    this.inputStreams = {};         // Trie (GLuint,ByteString)
}

Pipeline.prototype.createProgram = function(program) {
    // vertex shader
    var vs = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vs, program.vertexShader);
    gl.compileShader(vs);
    console.log('vs info: ' + gl.getShaderInfoLog(vs));
    // fragment shader
    var fs = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fs, program.fragmentShader);
    gl.compileShader(fs);
    console.log('fs info: ' + gl.getShaderInfoLog(fs));
    // program
    var p = gl.createProgram();
    gl.attachShader(p, vs);
    gl.attachShader(p, fs);
    gl.linkProgram(p);
    console.log('prog info: ' + gl.getProgramInfoLog(p));
    var prg = new GLProgram();
    prg.shaderObjects = [vs,fs];
    prg.programObject = p;
    // TODO
    return prg;
}


/*
data GLRenderTarget
    = GLRenderTarget
    { framebufferObject         :: GLuint
    , framebufferDrawbuffers    :: Maybe [GLenum]
    }
*/
function GLRenderTarget() {
    this.framebufferObject = 0;
    this.framebufferDrawbuffers = [];
}

Pipeline.prototype.createRenderTarget = function(target) {
}

/*
data GLTexture
    = GLTexture
    { glTextureObject   :: GLuint
    , glTextureTarget   :: GLenum
    }
*/
function GLTexture() {
    this.glTextureObject = 0;
    this.glTextureTarget = null;
}

Pipeline.prototype.createTexture = function(texture) {
    var texture = gl.createTexture();

/*

compileTexture :: TextureDescriptor -> IO GLTexture
compileTexture txDescriptor = do
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    let TextureDescriptor
            { textureType       = txType
            , textureSize       = txSize
            , textureSemantic   = txSemantic
            , textureSampler    = txSampler
            , textureBaseLevel  = txBaseLevel
            , textureMaxLevel   = txMaxLevel
            } = txDescriptor

        txSetup txTarget dTy = do
            let internalFormat  = fromIntegral $ textureDataTypeToGLType txSemantic dTy
                dataFormat      = fromIntegral $ textureDataTypeToGLArityType txSemantic dTy
            glBindTexture txTarget to
            glTexParameteri txTarget gl_TEXTURE_BASE_LEVEL $ fromIntegral txBaseLevel
            glTexParameteri txTarget gl_TEXTURE_MAX_LEVEL $ fromIntegral txMaxLevel
            setTextureSamplerParameters txTarget txSampler
            return (internalFormat,dataFormat)

        mipSize 0 x = [x]
        mipSize n x = x : mipSize (n-1) (x `div` 2)
        mipS = mipSize (txMaxLevel - txBaseLevel)
        levels = [txBaseLevel..txMaxLevel]
    target <- case txType of
        Texture2D dTy layerCnt -> do
            let VV2U (V2 txW txH) = txSize
                txTarget = gl_TEXTURE_2D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip3 levels (mipS txW) (mipS txH)) $ \(l,w,h) -> case layerCnt > 1 of
                True    -> glTexImage3D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) (fromIntegral layerCnt) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
                False   -> glTexImage2D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            return txTarget
        TextureCube dTy -> do
            let VV2U (V2 txW txH) = txSize
                txTarget = gl_TEXTURE_CUBE_MAP
                targets =
                    [ gl_TEXTURE_CUBE_MAP_POSITIVE_X 
                    , gl_TEXTURE_CUBE_MAP_NEGATIVE_X
                    , gl_TEXTURE_CUBE_MAP_POSITIVE_Y
                    , gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
                    , gl_TEXTURE_CUBE_MAP_POSITIVE_Z
                    , gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
                    ]
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip3 levels (mipS txW) (mipS txH)) $ \(l,w,h) -> 
                forM_ targets $ \t -> glTexImage2D t (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            return txTarget
    return $ GLTexture
        { glTextureObject   = to
        , glTextureTarget   = target
        }

*/
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
    this.gl.useProgram(this.glPrograms[index].programObject);
}

Pipeline.prototype.SetSamplerUniform = function(args) {
    console.log('TODO: implement SetSamplerUniform');
}

Pipeline.prototype.SetTexture = function(args) {
    console.log('TODO: implement SetTexture');
}

Pipeline.prototype.SetSampler = function(args) {
    console.log('ERROR: SetSampler command - Samplers are not supported by WebGL 1.0');
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
