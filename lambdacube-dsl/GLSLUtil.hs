module GLSLUtil where

import Language.GLSL.Syntax

varInit :: String -> TypeSpecifierNonArray -> Maybe TypeQualifier -> Maybe Expr -> Declaration
varInit name ty tq val = InitDeclaration (TypeDeclarator varType) [InitDecl name Nothing val]
  where
    varTySpecNoPrec = TypeSpecNoPrecision ty Nothing
    varTySpec = TypeSpec Nothing varTySpecNoPrec
    varType = FullType tq varTySpec

var :: String -> TypeSpecifierNonArray -> Maybe TypeQualifier -> Declaration
var name ty tq = varInit name ty tq Nothing

uniform :: String -> TypeSpecifierNonArray -> ExternalDeclaration
uniform name ty = Declaration $ var name ty (Just $ TypeQualSto Uniform)

inVar :: String -> TypeSpecifierNonArray -> ExternalDeclaration
inVar name ty = Declaration $ var name ty (Just $ TypeQualSto In)

inVarArr :: String -> InterpolationQualifier -> TypeSpecifierNonArray -> ExternalDeclaration
inVarArr name iq ty = Declaration $ InitDeclaration (TypeDeclarator varType) [InitDecl name (Just Nothing) Nothing]
  where
    tq = Just $ TypeQualInt iq $ Just In
    varTySpecNoPrec = TypeSpecNoPrecision ty Nothing
    varTySpec = TypeSpec Nothing varTySpecNoPrec
    varType = FullType tq varTySpec

inVarIQ :: String -> InterpolationQualifier -> TypeSpecifierNonArray -> ExternalDeclaration
inVarIQ name iq ty = Declaration $ var name ty (Just $ TypeQualInt iq $ Just In)

outVar :: String -> TypeSpecifierNonArray -> ExternalDeclaration
outVar name ty = Declaration $ var name ty (Just $ TypeQualSto Out)

outVarIQ :: String -> InterpolationQualifier -> TypeSpecifierNonArray -> ExternalDeclaration
outVarIQ name iq ty = Declaration $ var name ty (Just $ TypeQualInt iq $ Just Out)
{-
attribute :: String -> TypeSpecifierNonArray -> ExternalDeclaration
attribute name ty = Declaration $ var name ty (Just $ TypeQualSto Attribute)

varying :: String -> TypeSpecifierNonArray -> ExternalDeclaration
varying name ty = Declaration $ var name ty (Just $ TypeQualSto Varying)

varyingIQ :: String -> InterpolationQualifier -> TypeSpecifierNonArray -> ExternalDeclaration
varyingIQ name iq ty = Declaration $ var name ty (Just $ TypeQualInt iq $ Just Varying)
-}
assign :: Expr -> Expr -> Statement
assign l r = ExpressionStatement $ Just $ Equal l r

varStmt :: String -> TypeSpecifierNonArray -> Expr -> Statement
varStmt name ty val = DeclarationStatement $ varInit name ty Nothing $ Just val
