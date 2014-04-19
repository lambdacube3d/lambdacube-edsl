# WebGL backend based on OpenGL 3.3 Haskell LambdaCube backend

# utility functions

# Usage: zip(arr1, arr2, arr3, ...)
zip = () ->
  lengthArray = (arr.length for arr in arguments)
  length = Math.min(lengthArray...)
  for i in [0...length]
    arr[i] for arr in arguments


class Pipeline
    desc:               null    # Pipeline
    gl:                 null    # webgl backend
    glPrograms:         []      # Vector GLProgram
    glTextures:         []      # Vector GLTexture
    glSamplers:         []      # Vector GLSampler
    glTargets:          []      # Vector GLRenderTarget
    glCommands:         []      # [GLCommand]
    glSlotPrograms:     []      # Vector [ProgramName] -- programs depend on a slot
    glInput:            null    # IORef (Maybe InputConnection)
    glSlotNames:        []      # Vector ByteString
    glVAO:              null    # GLuint
    glTexUnitMapping:   []      # Trie (IORef GLint)   -- maps texture uniforms to texture units

    ####################
    # public interface
    ####################

    constructor: (@desc) ->

    allocate: (@gl) ->
        # create programs
        @glPrograms = (compileProgram p for p in @desc.programs)
        @glTextures = (compileTexture t for t in @desc.textures)
        @glCommands = @desc.commands
        console.log "allocated"

    dispose: ->
        # TODO

    bindInput: (input) ->
        # TODO

    render: ->
        # lookup and call the requested command
        @[cmd.tag](cmd.contents) for cmd in @glCommands
        console.log "rendered"
        "OK"

    ####################
    # internal functions
    ####################

    compileProgram = (program) ->
        # vertex shader
        vs = @gl.createShader(@gl.VERTEX_SHADER)
        @gl.shaderSource(vs, program.vertexShader)
        @gl.compileShader(vs)
        console.log('vs info: ' + @gl.getShaderInfoLog(vs))

        # fragment shader
        fs = @gl.createShader(@gl.FRAGMENT_SHADER)
        @gl.shaderSource(fs, program.fragmentShader)
        @gl.compileShader(fs)
        console.log('fs info: ' + @gl.getShaderInfoLog(fs))

        # program
        p = @gl.createProgram()
        @gl.attachShader(p, vs)
        @gl.attachShader(p, fs)
        @gl.linkProgram(p)
        console.log('prog info: ' + @gl.getProgramInfoLog(p))

        prg =
            shaderObjects:          [vs,fs]
            programObject:          p
            inputUniforms:          {} # TODO
            inputTextures:          {} # TODO
            inputTextureUniforms:   {} # TODO
            inputStreams:           {} # TODO

#data GLRenderTarget
#    = GLRenderTarget
#    { framebufferObject         :: GLuint
#    , framebufferDrawbuffers    :: Maybe [GLenum]
#    }
    compileRenderTarget = (target) ->
        # TODO

    compileTexture = (txDesc) ->
        texObj = @gl.createTexture()

        txType      = txDesc.textureType
        txSize      = txDesc.textureSize
        txSemantic  = txDesc.textureSemantic
        txSampler   = txDesc.textureSampler
        txBaseLevel = txDesc.textureBaseLevel
        txMaxLevel  = txDesc.textureMaxLevel

        txSetup = (txTarget,dTy) ->
            internalFormat  = textureDataTypeToGLType(txSemantic, dTy)
            dataFormat      = textureDataTypeToGLArityType(txSemantic, dTy)
            @gl.bindTexture(txTarget, texObj);
            @gl.texParameteri(txTarget, @gl.TEXTURE_BASE_LEVEL, txBaseLevel)
            @gl.texParameteri(txTarget, @gl.TEXTURE_MAX_LEVEL, txMaxLevel)
            setTextureSamplerParameters(txTarget, txSampler)
            [internalFormat,dataFormat]

        mipSize = (n,x) -> switch
            when n == 0 then [x]
            else mipSize(n-1, x // 2).unshift(x)
        mipS = (x) -> mipSize(txMaxLevel - txBaseLevel,x)
        levels = [txBaseLevel..txMaxLevel]

        target = switch txType.tag
            when 'Texture2D'
                txTarget = @gl.TEXTURE_2D
                [dTy, layerCnt] = txType.contents
                if txSize.tag == 'VV2U' and layerCnt == 1
                    [txW,txH] = txSize.contents
                    [internalFormat,dataFormat] = txSetup(txTarget, dTy)
                    for [l, w, h] in zip(levels, mipS(txW), mipS(txH))
                        @gl.texImage2D(txTarget, l, internalFormat, w, h, 0, dataFormat, @gl.UNSIGNED_BYTE, null)
                    txTarget
                else
                    console.log('ERROR - unsupported texture: ' + txType)
            when 'TextureCube'
                txTarget = @gl.TEXTURE_CUBE_MAP
                [dTy] = txType.contents
                if txSize.tag == 'VV2U'
                    [txW,txH] = txSize.contents
                    [internalFormat,dataFormat] = txSetup(txTarget, dTy)
                    targets =
                        [ @gl.TEXTURE_CUBE_MAP_POSITIVE_X 
                        , @gl.TEXTURE_CUBE_MAP_NEGATIVE_X
                        , @gl.TEXTURE_CUBE_MAP_POSITIVE_Y
                        , @gl.TEXTURE_CUBE_MAP_NEGATIVE_Y
                        , @gl.TEXTURE_CUBE_MAP_POSITIVE_Z
                        , @gl.TEXTURE_CUBE_MAP_NEGATIVE_Z
                        ]
                    for [l, w, h] in zip(levels, mipS(txW), mipS(txH))
                        for t in targets
                            @gl.texImage2D(t, l, internalFormat, w, h, 0, dataFormat, @gl.UNSIGNED_BYTE, null)
                    txTarget
                else
                    console.log('ERROR - unsupported texture: ' + txType)
            else
                console.log('ERROR - unsupported texture: ' + txType)
        texture =
            glTextureObject:    texObj
            glTextureTarget:    target

    # pipeline command implementation
    SetRasterContext: (args) ->
        console.log('TODO: implement SetRasterContext')

    SetAccumulationContext: (args) ->
        console.log('TODO: implement SetAccumulationContext')

    SetRenderTarget: (index) ->
        @gl.bindFramebuffer(@gl.FRAMEBUFFER, null)

    SetProgram: (index) ->
        @gl.useProgram(@glPrograms[index].programObject)

    SetSamplerUniform: (args) ->
        console.log('TODO: implement SetSamplerUniform')

    SetTexture: (args) ->
        console.log('TODO: implement SetTexture')

    SetSampler: (args) ->
        console.log('ERROR: SetSampler command - Samplers are not supported by WebGL 1.0')

    RenderSlot: (args) ->
        console.log('TODO: implement RenderSlot')

    ClearRenderTarget: (pairs) ->
        mask = 0
        for [cmd, arg] in pairs
            switch cmd
                when 'Color'
                    mask |= @gl.COLOR_BUFFER_BIT
                    if arg.tag == 'VV4F'
                        color = arg.contents
                        @gl.clearColor(color[0], color[1], color[2], color[3])
                    else
                        console.log('TODO: implement ClearRenderTarget for formats other than VV4F');
                        @gl.clearColor(0, 0, 1, 1);
                when 'Depth'
                    mask |= @gl.DEPTH_BUFFER_BIT
                    console.log('TODO: implement ClearRenderTarget for Depth value')
                when 'Stencil'
                    mask |= @gl.STENCIL_BUFFER_BIT
                    console.log('TODO: implement ClearRenderTarget for Stencil value')
                else
                    console.log('error - invalid clear command' + cmd)
        @gl.clear(mask)

    GenerateMipMap: (args) ->
        console.log('TODO: implement GenerateMipMap')

    SaveImage: (args) ->
        console.log('TODO: implement SaveImage')

    LoadImage: (args) ->
        console.log('TODO: implement LoadImage')

#class PipelineInput
#    PipelineInput.prototype.allocate = (gl) {
#    PipelineInput.prototype.dispose = function() {

# export classes
window.Pipeline = Pipeline
