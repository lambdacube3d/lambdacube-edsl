# WebGL 1.0 backend based on OpenGL 3.3 Haskell LambdaCube backend

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

        ###
    -- check program input
    (uniforms,uniformsType) <- queryUniforms po
    (attributes,attributesType) <- queryStreams po
    print uniforms
    print attributes
    when (uniformsType /= programUniforms p `unionL` programInTextures p) $ fail "shader program uniform input mismatch!"
    when (attributesType /= fmap snd (programStreams p)) $ fail "shader program stream input mismatch!"
    -- the public (user) pipeline and program input is encoded by the slots, therefore the programs does not distinct the render and slot textures input
    let inUniNames = programUniforms p
        (inUniforms,inTextures) = L.partition (\(n,v) -> T.member n inUniNames) $ T.toList $ uniforms
        texUnis = [n | (n,_) <- inTextures, T.member n uniTrie]
    return $ GLProgram
        { shaderObjects         = objs
        , programObject         = po
        , inputUniforms         = T.fromList inUniforms
        , inputTextures         = T.fromList inTextures
        , inputTextureUniforms  = S.fromList texUnis
        , inputStreams          = T.fromList [(n,(idx,attrName)) | ((n,idx),(_,(attrName,_))) <- zip (T.toList attributes) (T.toList $ programStreams p)]
        }
        ###
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
            #@gl.texParameteri(txTarget, @gl.TEXTURE_BASE_LEVEL, txBaseLevel)
            #@gl.texParameteri(txTarget, @gl.TEXTURE_MAX_LEVEL, txMaxLevel)
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

    #textureDataTypeToGLType :: ImageSemantic -> TextureDataType -> GLenum
    textureDataTypeToGLType = (s, t) ->
        error = () -> console.log('error - invalid texture data type: ' + s + ' ' + t)
        switch s
            when 'Color' then switch t.tag
                when 'FloatT' then switch t.contents
                    #when 'Red'  then @gl.R32F
                    #when 'RG'   then @gl.RG32F
                    when 'RGBA' then @gl.RGBA
                    else error()
                #when 'IntT' then switch t.contents
                #    when 'Red'  then @gl.R32I
                #    when 'RG'   then @gl.RG32I
                #    when 'RGBA' then @gl.RGBA32I
                #    else error()
                #when 'WordT' then switch t.contents
                #    when 'Red'  then @gl.R32UI
                #    when 'RG'   then @gl.RG32UI
                #    when 'RGBA' then @gl.RGBA32UI
                #    else error()
                else error()
            #when 'Depth' then switch t.tag
            #    when 'FloatT' then switch t.contents
            #        when 'Red'  then @gl.DEPTH_COMPONENT32F
            #        else error()
            #    when 'WordT' then switch t.contents
            #        when 'Red'  then @gl.DEPTH_COMPONENT32
            #        else error()
            #    else error()
            else error()

    #textureDataTypeToGLArityType :: ImageSemantic -> TextureDataType -> GLenum
    textureDataTypeToGLArityType = (s, t) ->
        error = () -> console.log('error - invalid texture data type: ' + s + ' ' + t)
        switch s
            when 'Color' then switch t.tag
                when 'FloatT' then switch t.contents
                    #when 'Red'  then @gl.LUMINANCE
                    #when 'RG'   then @gl.LUMINANCE_ALPHA
                    when 'RGBA' then @gl.RGBA
                    else error()
                #when 'IntT' then switch t.contents
                #    when 'Red'  then @gl.LUMINANCE
                #    when 'RG'   then @gl.LUMINANCE_ALPHA
                #    when 'RGBA' then @gl.RGBA
                #    else error()
                #when 'WordT' then switch t.contents
                #    when 'Red'  then @gl.LUMINANCE
                #    when 'RG'   then @gl.LUMINANCE_ALPHA
                #    when 'RGBA' then @gl.RGBA
                #    else error()
                else error()
            #when 'Depth' then switch t.tag
            #    when 'FloatT' then switch t.contents
            #        when 'Red'  then @gl.DEPTH_COMPONENT
            #        else error()
            #    when 'WordT' then switch t.contents
            #        when 'Red'  then @gl.DEPTH_COMPONENT
            #        else error()
            #    else error()
            else error()

    #edgeModeToGLType :: EdgeMode -> GLenum
    edgeModeToGLType = (a) -> switch a
        when 'Repeat'           then @gl.REPEAT
        when 'MirroredRepeat'   then @gl.MIRRORED_REPEAT
        when 'ClampToEdge'      then @gl.CLAMP_TO_EDGE
        else console.log('error - invalid texture edge mode: ' + a)

    #filterToGLType :: Filter -> GLenum
    filterToGLType = (a) -> switch a
        when 'Nearest'              then @gl.NEAREST
        when 'Linear'               then @gl.LINEAR
        when 'NearestMipmapNearest' then @gl.NEAREST_MIPMAP_NEAREST
        when 'NearestMipmapLinear'  then @gl.NEAREST_MIPMAP_LINEAR
        when 'LinearMipmapNearest'  then @gl.LINEAR_MIPMAP_NEAREST
        when 'LinearMipmapLinear'   then @gl.LINEAR_MIPMAP_LINEAR

    #comparisonFunctionToGLType :: ComparisonFunction -> GLenum
    comparisonFunctionToGLType = (a) -> switch a
        when 'Always'   then @gl.ALWAYS
        when 'Equal'    then @gl.EQUAL
        when 'Gequal'   then @gl.GEQUAL
        when 'Greater'  then @gl.GREATER
        when 'Lequal'   then @gl.LEQUAL
        when 'Less'     then @gl.LESS
        when 'Never'    then @gl.NEVER
        when 'Notequal' then @gl.NOTEQUAL


    #setTextureSamplerParameters :: GLenum -> SamplerDescriptor -> IO ()
    setTextureSamplerParameters = (t,s) ->
        @gl.texParameteri(t, @gl.TEXTURE_WRAP_S, edgeModeToGLType(s.samplerWrapS))
        if s.samplerWrapT?
            @gl.texParameteri(t, @gl.TEXTURE_WRAP_T, edgeModeToGLType(s.samplerWrapT))
        else
            console.log('error - TEXTURE_WRAP_T is missing: ' + s + ' ' + t)

        @gl.texParameteri(t, @gl.TEXTURE_MIN_FILTER, filterToGLType(s.samplerMinFilter))
        @gl.texParameteri(t, @gl.TEXTURE_MAG_FILTER, filterToGLType(s.samplerMagFilter))


    # pipeline command implementation
    SetRasterContext: (ctx) ->
        cff = (a) -> switch a
            when 'CCW'  then    @gl.CCW
            when 'CW'   then    @gl.CW
            else console.log('error - unknown FrontFace: ' + a)

        switch ctx.tag
            when 'PointCtx' then null
            when 'LineCtx'
                [lw, pv] = ctx.contents
                @gl.LineWidth(lw)
            when 'TriangleCtx'
                [cm, pm, po, pv] = ctx.contents
                # cull mode
                switch cm.tag
                    when 'CullNone'
                        @gl.disable(@gl.CULL_FACE)
                    when 'CullFront'
                        [f] = cm.contents
                        @gl.enable(@gl.CULL_FACE)
                        @gl.cullFace(@gl.FRONT)
                        @gl.frontFace(cff(f))
                    when 'CullBack'
                        [f] = cm.contents
                        @gl.enable(@gl.CULL_FACE)
                        @gl.cullFace(@gl.BACK)
                        @gl.frontFace(cff(f))
                    else console.log('error - invalid CullMode: ' + cm.tag)
                # polygon offset
                switch po.tag
                    when 'NoOffset'
                        @gl.disable(@gl.POLYGON_OFFSET_FILL)
                    when 'Offset'
                        [f,u] = po.contents
                        @gl.polygonOffset(f,u)
                        @gl.enable(@gl.POLYGON_OFFSET_FILL)
                    else console.log('error - invalid PolygonOffset: ' + po.tag)
            else console.log('error - invalid RasterContext: ' + ctx.tag)

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

# utility functions

# Usage: zip(arr1, arr2, arr3, ...)
zip = () ->
  lengthArray = (arr.length for arr in arguments)
  length = Math.min(lengthArray...)
  for i in [0...length]
    arr[i] for arr in arguments

###
compileRenderTarget :: Vector TextureDescriptor -> Vector GLTexture -> RenderTarget -> IO GLRenderTarget
compileRenderTarget texs glTexs (RenderTarget targets) = do
    let isFB (Framebuffer _)    = True
        isFB _                  = False
        images = [img | (_,Just img) <- targets]
    case all isFB images of
        True -> do
            let bufs = [cvt img | (Color,img) <- targets]
                cvt a = case a of
                    Nothing                     -> gl_NONE
                    Just (Framebuffer Color)    -> gl_BACK_LEFT
                    _                           -> error "internal error (compileRenderTarget)!"
            return $ GLRenderTarget
                { framebufferObject         = 0
                , framebufferDrawbuffers    = Just bufs
                }
        False -> do
            when (any isFB images) $ fail "internal error (compileRenderTarget)!"
            fbo <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
            glBindFramebuffer gl_DRAW_FRAMEBUFFER fbo
            let attach attachment (TextureImage texIdx level (Just layer)) =
                    glFramebufferTextureLayer gl_DRAW_FRAMEBUFFER attachment (glTextureTarget $ glTexs ! texIdx) (fromIntegral level) (fromIntegral layer)
                attach attachment (TextureImage texIdx level Nothing) = do
                    let glTex = glTexs ! texIdx
                        tex = texs ! texIdx
                        txLevel = fromIntegral level
                        txTarget = glTextureTarget glTex
                        txObj = glTextureObject glTex
                        attachArray = glFramebufferTexture gl_DRAW_FRAMEBUFFER attachment txObj txLevel
                        attach2D    = glFramebufferTexture2D gl_DRAW_FRAMEBUFFER attachment txTarget txObj txLevel
                    case textureType tex of
                        Texture1D     _ n
                            | n > 1             -> attachArray
                            | otherwise         -> glFramebufferTexture1D gl_DRAW_FRAMEBUFFER attachment txTarget txObj txLevel
                        Texture2D     _ n
                            | n > 1             -> attachArray
                            | otherwise         -> attach2D
                        Texture3D     _         -> attachArray
                        TextureCube   _         -> attachArray
                        TextureRect   _         -> attach2D
                        Texture2DMS   _ n _ _
                            | n > 1             -> attachArray
                            | otherwise         -> attach2D
                        TextureBuffer _         -> fail "internalError (compileRenderTarget/TextureBuffer)!"

                go a (Stencil,Just img) = do
                    fail "Stencil support is not implemented yet!"
                    return a
                go a (Depth,Just img) = do
                    attach gl_DEPTH_ATTACHMENT img
                    return a
                go (bufs,colorIdx) (Color,Just img) = do
                    let attachment = gl_COLOR_ATTACHMENT0 + fromIntegral colorIdx
                    attach attachment img
                    return (attachment : bufs, colorIdx + 1)
                go (bufs,colorIdx) (Color,Nothing) = return (gl_NONE : bufs, colorIdx + 1)
                go a _ = return a
            (bufs,_) <- foldM go ([],0) targets
            withArray (reverse bufs) $ glDrawBuffers (fromIntegral $ length bufs)
            return $ GLRenderTarget
                { framebufferObject         = fbo
                , framebufferDrawbuffers    = Nothing
                }
###
