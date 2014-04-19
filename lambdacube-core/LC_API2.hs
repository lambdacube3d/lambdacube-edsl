module LC_API2 (
    -- language
    module LC_G_Type,
    module LC_G_APIType,
    module LC_T_APIType,
    module LC_T_DSLType,
    module LC_T_HOAS,
    module LC_T_Language,
    module LC_B2_IR,
    Int32,
    Word32,

    compilePipeline
) where

import Data.Int
import Data.Word

import LC_G_APIType hiding (InputType(..))
import LC_G_Type

import LC_T_APIType
import LC_T_DSLType hiding (Buffer,Shadow)
import LC_T_HOAS
import LC_T_Language
import qualified LC_T_APIType as H
import qualified LC_T_HOAS as H

import qualified LC_U_DeBruijn as U
import LC_C_Convert

import LC_B2_IR
import qualified LC_B2_Compile3 as B2

import Control.Monad.State

compilePipeline :: TargetPlatform -> H.GPOutput H.SingleOutput -> Either String Pipeline
compilePipeline t l = B2.compile t dag
  where
    (_, dag') = runState (U.unN $ convertGPOutput l) U.emptyDAG
    dag = U.addCount dag'
