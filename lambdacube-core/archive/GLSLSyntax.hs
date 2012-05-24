{-
Copyright: (c) Vo Minh Thu, 2010.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module GLSLSyntax where

----------------------------------------------------------------------
-- Abstract syntax tree
----------------------------------------------------------------------

-- at least one
data TranslationUnit
    = TranslationUnit [ExternalDeclaration]
    deriving (Show, Eq)

data ExternalDeclaration
    -- function declarations should be at top level (page 28)
    = FunctionDeclaration FunctionPrototype
    | FunctionDefinition FunctionPrototype Compound
    | Declaration Declaration
    deriving (Show, Eq)

-- TODO clean
data Declaration
-- e.g. layout (origin_upper_left) in vec4 gl_FragCoord;
--      struct name { ... };
--      struct name { ... } name;
    = InitDeclaration InvariantOrType [InitDeclarator]
    | Precision PrecisionQualifier TypeSpecifierNoPrecision
    | Block TypeQualifier String [Field] (Maybe (String, Maybe (Maybe Expr))) -- constant expression
-- e.g. layout (origin_upper_left) in; TODO check if it is only used for default layout.
    | TQ TypeQualifier
    deriving (Show, Eq)

-- TODO regroup String (Maybe (Maybe Expr)) as Declarator and use it for
-- StructDeclarator.
data InitDeclarator
    = InitDecl String (Maybe (Maybe Expr)) (Maybe Expr) -- constant expression; assignment expression
    deriving (Show, Eq)

data InvariantOrType
    = InvariantDeclarator
    | TypeDeclarator FullType
    deriving (Show, Eq)

data FunctionPrototype
    = FuncProt FullType String [ParameterDeclaration]
    deriving (Show, Eq)

data ParameterDeclaration
    = ParameterDeclaration  (Maybe ParameterTypeQualifier)
                            (Maybe ParameterQualifier)
                            TypeSpecifier
                            (Maybe (String, Maybe Expr)) -- constant expression
    deriving (Show, Eq)

data FullType
    = FullType (Maybe TypeQualifier) TypeSpecifier
    deriving (Show, Eq)

-- sto
-- lay [sto]
-- int [sto]
-- inv [sto]
-- inv int sto
data TypeQualifier
    = TypeQualSto StorageQualifier
    | TypeQualLay LayoutQualifier (Maybe StorageQualifier)
    | TypeQualInt InterpolationQualifier (Maybe StorageQualifier)
    | TypeQualInv InvariantQualifier (Maybe StorageQualifier)
    | TypeQualInv3 InvariantQualifier InterpolationQualifier StorageQualifier
    deriving (Show, Eq)

data TypeSpecifier
    = TypeSpec (Maybe PrecisionQualifier) TypeSpecifierNoPrecision
    deriving (Show, Eq)

data InvariantQualifier
    = Invariant
    deriving (Show, Eq)

data InterpolationQualifier
    = Smooth
    | Flat
    | NoPerspective
    deriving (Show, Eq)

data LayoutQualifier
    = Layout [LayoutQualifierId]
    deriving (Show, Eq)

data LayoutQualifierId
    = LayoutQualId String (Maybe Expr) -- TODO Expr should be IntConstant
    deriving (Show, Eq)

data Statement
  -- declaration statement
    = DeclarationStatement Declaration
  -- jump statement
    | Continue
    | Break
    | Return (Maybe Expr)
    | Discard -- fragment shader only
  -- compound statement
    | CompoundStatement Compound
  -- expression statement
    | ExpressionStatement (Maybe Expr)
  -- selection statement
    | SelectionStatement Expr Statement (Maybe Statement)
  -- switch statement
    | SwitchStatement Expr [Statement]
    | CaseLabel CaseLabel
  -- iteration statement
    | While Condition Statement -- no new scope
    | DoWhile Statement Expr
    | For (Either (Maybe Expr) Declaration) (Maybe Condition) (Maybe Expr) Statement
    -- 1st stmt: expression or declaration, 2nd: no new scope
    deriving (Show, Eq)

data Compound
    = Compound [Statement]
    deriving (Show, Eq)

data Condition
    = Condition Expr
    | InitializedCondition FullType String Expr -- assignment expression
    deriving (Show, Eq)

data CaseLabel
    = Case Expr
    | Default
    deriving (Show, Eq)

data StorageQualifier
    = Const
    | Attribute -- vertex only
    | Varying
    | CentroidVarying
    | In
    | Out
    | CentroidIn
    | CentroidOut
    | Uniform
    deriving (Show, Eq)

data TypeSpecifierNoPrecision
    = TypeSpecNoPrecision TypeSpecifierNonArray (Maybe (Maybe Expr)) -- constant expression
    deriving (Show, Eq)

data TypeSpecifierNonArray
    = Void
    | Float
    | Int
    | UInt
    | Bool
    | BVec2
    | BVec3
    | BVec4
    | IVec2
    | IVec3
    | IVec4
    | Mat2
    | Mat2x2
    | Mat2x3
    | Mat2x4
    | Mat3
    | Mat3x2
    | Mat3x3
    | Mat3x4
    | Mat4
    | Mat4x2
    | Mat4x3
    | Mat4x4
    | UVec2
    | UVec3
    | UVec4
    | Vec2
    | Vec3
    | Vec4
    | ISampler1D
    | ISampler1DArray
    | ISampler2D
    | ISampler2DArray
    | ISampler2DMS
    | ISampler2DMSArray
    | ISampler2DRect
    | ISampler3D
    | ISamplerBuffer
    | ISamplerCube
    | Sampler1D
    | Sampler1DArray
    | Sampler1DArrayShadow
    | Sampler1DShadow
    | Sampler2D
    | Sampler2DArray
    | Sampler2DArrayShadow
    | Sampler2DMS
    | Sampler2DMSArray
    | Sampler2DRect
    | Sampler2DRectShadow
    | Sampler2DShadow
    | Sampler3D
    | SamplerBuffer
    | SamplerCube
    | SamplerCubeShadow
    | USampler1D
    | USampler1DArray
    | USampler2D
    | USampler2DArray
    | USampler2DMS
    | USampler2DMSArray
    | USampler2DRect
    | USampler3D
    | USamplerBuffer
    | USamplerCube
    | StructSpecifier (Maybe String) [Field]
    | TypeName String -- TODO user-defined type, should verify if it is declared
    deriving (Show, Eq)

data PrecisionQualifier
    = HighP
    | MediumP
    | LowP
    deriving (Show, Eq)

-- TODO The type qualifier can be present only when there is one or more declarators.
-- There other restrictions, see 4.1.8.
data Field
    = Field (Maybe TypeQualifier) TypeSpecifier [StructDeclarator]
    deriving (Show, Eq)

data StructDeclarator
    = StructDeclarator String (Maybe (Maybe Expr)) -- constant expression
    deriving (Show, Eq)

data Expr
  -- primaryExpression
    = Variable String
    | IntConstant IntConstantKind Integer
    | FloatConstant Float
    | BoolConstant Bool
  -- postfixExpression
    | Bracket Expr Expr
    | FieldSelection Expr String
    | MethodCall Expr FunctionIdentifier Parameters
    | FunctionCall FunctionIdentifier Parameters
    | PostInc Expr
    | PostDec Expr
    | PreInc Expr
    | PreDec Expr
  -- unary expression
    | UnaryPlus Expr
    | UnaryNegate Expr
    | UnaryNot Expr
    | UnaryOneComplement Expr
  -- binary expression
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Add Expr Expr
    | Sub Expr Expr
    | LeftShift Expr Expr
    | RightShift Expr Expr
    | Lt Expr Expr
    | Gt Expr Expr
    | Lte Expr Expr
    | Gte Expr Expr
    | Equ Expr Expr
    | Neq Expr Expr
    | BitAnd Expr Expr
    | BitXor Expr Expr
    | BitOr Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | Selection Expr Expr Expr -- ternary _ ? _ : _ operator
  -- assignment, the left Expr should be unary expression
    | Equal Expr Expr
    | MulAssign Expr Expr
    | DivAssign Expr Expr
    | ModAssign Expr Expr
    | AddAssign Expr Expr
    | SubAssign Expr Expr
    | LeftAssign Expr Expr
    | RightAssign Expr Expr
    | AndAssign Expr Expr
    | XorAssign Expr Expr
    | OrAssign Expr Expr
  -- sequence
    | Sequence Expr Expr
    deriving (Show, Eq)

data IntConstantKind
    = Hexadecimal
    | Octal
    | Decimal
    deriving (Show, Eq)

data Parameters
    = ParamVoid
    | Params [Expr]
    deriving (Show, Eq)

data ParameterQualifier
    = InParameter
    | OutParameter
    | InOutParameter
    deriving (Show, Eq)

data ParameterTypeQualifier
    = ConstParameter
    deriving (Show, Eq)

data FunctionIdentifier
    -- TODO could be refine (I think a precision qualifier is not permitted,
    -- nor a complete struct definition)
    = FuncIdTypeSpec TypeSpecifier
    | FuncId String
    deriving (Show, Eq)
