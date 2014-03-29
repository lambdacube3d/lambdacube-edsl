var testIR = {
    "textures": [],
    "commands": [
        {
            "tag": "SetRenderTarget",
            "contents": 0
        },
        {
            "tag": "ClearRenderTarget",
            "contents": [
                [
                    "Color",
                    {
                        "tag": "VV4F",
                        "contents": [
                            1,
                            0,
                            0,
                            1
                        ]
                    }
                ]
            ]
        },
        {
            "tag": "SetRasterContext",
            "contents": {
                "tag": "TriangleCtx",
                "contents": [
                    {
                        "tag": "CullNone",
                        "contents": []
                    },
                    {
                        "tag": "PolygonFill",
                        "contents": []
                    },
                    {
                        "tag": "NoOffset",
                        "contents": []
                    },
                    "LastVertex"
                ]
            }
        },
        {
            "tag": "SetAccumulationContext",
            "contents": {
                "accViewportName": null,
                "accOperations": [
                    {
                        "tag": "ColorOp",
                        "contents": [
                            {
                                "tag": "NoBlending",
                                "contents": []
                            },
                            {
                                "tag": "VV4B",
                                "contents": [
                                    true,
                                    true,
                                    true,
                                    true
                                ]
                            }
                        ]
                    }
                ]
            }
        },
        {
            "tag": "SetProgram",
            "contents": 0
        },
        {
            "tag": "SetSamplerUniform",
            "contents": [
                "ScreenQuad",
                0
            ]
        },
        {
            "tag": "RenderSlot",
            "contents": 0
        }
    ],
    "slots": [
        {
            "slotPrimitive": "Triangles",
            "slotStreams": [
                [
                    "position",
                    "V2F"
                ]
            ],
            "slotName": "postSlot",
            "slotUniforms": [
                [
                    "ScreenQuad",
                    "FTexture2D"
                ]
            ],
            "slotPrograms": [
                0
            ]
        }
    ],
    "programs": [
        {
            "programInTextures": [
                [
                    "ScreenQuad",
                    "FTexture2D"
                ]
            ],
            "programOutput": [
                [
                    "f0",
                    "V4F"
                ]
            ],
            "programStreams": [
                [
                    "vAttributeIn_0",
                    [
                        "position",
                        "V2F"
                    ]
                ]
            ],
            "fragmentShader": "precision mediump float;\nuniform sampler2D ScreenQuad;\nvarying vec2 v0;\nvoid main ()\n{ float val4 = 0.5;\n gl_FragColor = texture2D (ScreenQuad, v0 * val4 + val4);\n}\n",
            "vertexShader": "attribute vec2 vAttributeIn_0;\nvarying vec2 v0;\nvoid main ()\n{ float val17 = 1.0;\n  gl_Position = vec4 (vAttributeIn_0.x, vAttributeIn_0.y, val17, val17);\n  gl_PointSize = val17;\n  v0 = vAttributeIn_0;\n}\n",
            "geometryShader": null,
            "programUniforms": []
        }
    ],
    "samplers": [],
    "targets": [
        {
            "renderTargets": [
                [
                    "Color",
                    {
                        "tag": "Framebuffer",
                        "contents": "Color"
                    }
                ]
            ]
        }
    ]
};