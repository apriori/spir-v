{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module SpirV.Term where

import           Data.Bits
import           Data.Char
import           Data.DoubleWord
import qualified Data.Vector               as V
import           Data.Word
import           GHC.Int

import           Language.Haskell.TH
import           SpirV.AccessQualifier
import           SpirV.AddressingModel
import           SpirV.Decoration
import           SpirV.ExecutionMode
import           SpirV.ExecutionModel
import           SpirV.FunctionControlMask
import           SpirV.Magic
import           SpirV.MemoryModel
import           SpirV.OpCode
import           SpirV.SourceLanguage
import           SpirV.StorageClass

data ID = ID Word32
data TypeID = TypeID Word32
data Term
  = Term ID

tid :: TypeID -> Word32
tid (TypeID t) = t

data WordCount = WordCount Int16
data Result = Result ID
data Operand = Operand Word32
             | LiteralS String
             | LiteralI Integer
             | LiteralW Word32
             | Type TypeID
             | ParamType TypeID
             | ResultOperand Result
             | ResultType Word32
data OpCode = OpCode ID
data Instr = Instr WordCount OpCode (Maybe Result) (Maybe TypeID) [Operand]
           | SrcInstr WordCount OpCode SourceLanguage Operand

data TypeOp
  = BoolType OpCode
  | IntType OpCode


data Signedness = Unsigned | Signed deriving (Enum)

declareAllOps
genAllOpInstances

class Instruction repr where
  opNop :: OpNop repr
 -- opAccessChain :: OpAccessChain repr
  -- opAll
  -- opAny
  -- opArrayLength
  -- opAsyncGroupCopy
  -- opAtomicAnd
  -- opAtomicCompareExchange
  -- opAtomicCompareExchangeWeak
  -- opAtomicExchange
  -- opAtomicIAdd
  -- opAtomicIDecrement
  -- opAtomicIIncrement
  -- opAtomicInit
  -- opAtomicISub
  -- opAtomicLoad
  -- opAtomicOr
  -- opAtomicStore
  -- opAtomicUMax
  -- opAtomicUMin
  -- opAtomicXor
  -- opBitcast
  -- opBitwiseAnd
  -- opBitwiseOr
  -- opBitwiseXor
  -- opBranch ::
  -- opBranchConditional
  -- opBuildNDRange
  -- opCaptureEventProfilingInfo
  -- opCommitReadPipe
  -- opCommitWritePipe
  -- opCompileFlag
  -- opCompositeConstruct
  -- opCompositeExtract
  -- opCompositeInsert
  -- opConstant
  -- opConstantComposite
  -- opConstantFalse
  -- opConstantNullObject
  -- opConstantNullPointer
  -- opConstantSampler
  -- opConstantTrue
  -- opControlBarrier
  -- opConvertFToS
  -- opConvertFToU
  -- opConvertPtrToU
  -- opConvertSToF
  -- opConvertUToF
  -- opConvertUToPtr
  -- opCopyMemory
  -- opCopyMemorySized
  -- opCopyObject
  -- opCreateUserEvent
  -- opDecorate
  -- opDecorationGroup
  -- opDot
  -- opDPdx
  -- opDPdxCoarse
  -- opDPdxFine
  -- opDPdy
  -- opDPdyCoarse
  -- opDPdyFine
  -- opEmitStreamVertex
  -- opEmitVertex
  -- opEndPrimitive
  -- opEndStreamPrimitive
  -- opEnqueueKernel
  -- opEnqueueMarker
  -- opEntryPoint
  -- opExecutionMode
  -- opExtension
  -- opExtInst
  -- opExtInstImport
  -- opFAdd
  -- opFConvert
  -- opFDiv
  -- opFMod
  -- opFMul
  -- opFNegate
  -- opFOrdEqual
  -- opFOrdGreaterThan
  -- opFOrdGreaterThanEqual
  -- opFOrdLessThan
  -- opFOrdLessThanEqual
  -- opFOrdNotEqual
  -- opFRem
  -- opAccessChain
  -- opAll
  -- opAny
  -- opArrayLength
  -- opAsyncGroupCopy
  -- opAtomicAnd
  -- opAtomicCompareExchange
  -- opAtomicCompareExchangeWeak
  -- opAtomicExchange
  -- opAtomicIAdd
  -- opAtomicIDecrement
  -- opAtomicIIncrement
  -- opAtomicInit
  -- opAtomicISub
  -- opAtomicLoad
  -- opAtomicOr
  -- opAtomicStore
  -- opAtomicUMax
  -- opAtomicUMin
  -- opAtomicXor
  -- opBitcast
  -- opBitwiseAnd
  -- opBitwiseOr
  -- opBitwiseXor
  opBranch :: ID -> OpBranch repr
  -- opBranchConditional
  -- opBuildNDRange
  -- opCaptureEventProfilingInfo
  -- opCommitReadPipe
  -- opCommitWritePipe
  -- opCompileFlag
  -- opCompositeConstruct
  -- opCompositeExtract
  -- opCompositeInsert
  -- opConstant
  -- opConstantComposite
  opConstantFalse :: ID -> ID -> OpConstantFalse repr
  -- opConstantNullObject
  -- opConstantNullPointer
  -- opConstantSampler
  opConstantTrue :: ID -> ID -> OpConstantTrue repr
  -- opControlBarrier
  -- opConvertFToS
  -- opConvertFToU
  -- opConvertPtrToU
  -- opConvertSToF
  -- opConvertUToF
  -- opConvertUToPtr
  -- opCopyMemory
  -- opCopyMemorySized
  -- opCopyObject
  -- opCreateUserEvent
  --opDecorate :: ID ->
  -- opDecorationGroup
  -- opDot
  -- opDPdx
  -- opDPdxCoarse
  -- opDPdxFine
  -- opDPdy
  -- opDPdyCoarse
  -- opDPdyFine
  -- opEmitStreamVertex
  -- opEmitVertex
  -- opEndPrimitive
  -- opEndStreamPrimitive
  -- opEnqueueKernel
  -- opEnqueueMarker
  opEntryPoint :: ExecutionModel -> Result -> String -> OpEntryPoint repr
  --opExecutionMode ::
  opExtension :: String -> OpExtension repr
  opExtInstImport :: Result -> String -> OpExtInstImport repr
  -- opExtInst ::
  -- opFAdd
  -- opFConvert
  -- opFDiv
  -- opFMod
  -- opFMul
  -- opFNegate
  -- opFOrdEqual
  -- opFOrdGreaterThan
  -- opFOrdGreaterThanEqual
  -- opFOrdLessThan
  -- opFOrdLessThanEqual
  -- opFOrdNotEqual
  -- opFRem
  -- opFSub
  opFunction :: TypeID -> ID -> FunctionControlMask -> ID -> OpFunction repr
  -- opFunctionCall
  opFunctionEnd :: OpFunctionEnd repr
  opFunctionParameter :: ID -> ID -> OpFunctionParameter repr
  -- opFUnordEqual
  -- opFUnordGreaterThan
  -- opFUnordGreaterThanEqual
  -- opFUnordLessThan
  -- opFUnordLessThanEqual
  -- opFUnordNotEqual
  -- opFwidth
  -- opFwidthCoarse
  -- opFwidthFine
  -- opGenericCastToPtr
  -- opGenericCastToPtrExplicit
  -- opGenericPtrMemSemantics
  -- opGetDefaultQueue
  -- opGetKernelNDrangeMaxSubGroupSize
  -- opGetKernelNDrangeSubGroupCount
  -- opGetKernelPreferredWorkGroupSizeMultiple
  -- opGetKernelWorkGroupSize
  -- opGetMaxPipePackets
  -- opGetNumPipePackets
  -- opGroupAll
  -- opGroupAny
  -- opGroupBroadcast
  -- opGroupCommitReadPipe
  -- opGroupCommitWritePipe
  -- opGroupDecorate
  -- opGroupFAdd
  -- opGroupFMax
  -- opGroupFMin
  -- opGroupIAdd
  -- opGroupMemberDecorate
  -- opGroupReserveReadPipePackets
  -- opGroupReserveWritePipePackets
  -- opGroupSMax
  -- opGroupSMin
  -- opGroupUMax
  -- opGroupUMin
  -- opIAdd
  -- opIEqual
  -- opImagePointer
  -- opIMul
  -- opInBoundsAccessChain
  -- opINotEqual
  -- opIsFinite
  -- opIsInf
  -- opIsNan
  -- opIsNormal
  -- opISub
  -- opIsValidEvent
  -- opIsValidReserveId
  -- opKill
  opLabel :: ID -> OpLabel repr
  -- opLessOrGreater
  -- opLifetimeStart
  -- opLifetimeStop
  opLine :: Int -> Int -> ID -> Result -> OpLine repr
  -- opLoad
  -- opLogicalAnd
  -- opLogicalOr
  -- opLogicalXor
  -- opLoopMerge
  -- opMatrixTimesMatrix
  -- opMatrixTimesScalar
  -- opMatrixTimesVector
  -- opMemberDecorate
  opMemberName :: String -> TypeOp -> OpMemberName repr
  -- opMemoryBarrier
  opMemoryModel :: AddressingModel -> MemoryModel -> OpMemoryModel repr
  opName :: String -> ID -> OpName repr
  --opNot :: TypeID -> ID ->
  -- opOrdered
  -- opOuterProduct
  -- opPhi
  -- opPtrCastToGeneric
  -- opReadPipe
  -- opReleaseEvent
  -- opReservedReadPipe
  -- opReservedWritePipe
  -- opReserveReadPipePackets
  -- opReserveWritePipePackets
  -- opRetainEvent
  opReturn :: OpReturn repr
  -- opReturnValue
  -- opSampler
  -- opSConvert
  -- opSDiv
  -- opSelect
  -- opSelectionMerge
  -- opSetUserEventStatus
  -- opSGreaterThan
  -- opSGreaterThanEqual
  -- opShiftLeftLogical
  -- opShiftRightArithmetic
  -- opShiftRightLogical
  -- opSignBitSet
  -- opSLessThan
  -- opSLessThanEqual
  -- opSMod
  -- opSNegate
  --  SrcInstr WordCount OpCode SourceLanguage Operand
  opSource :: SourceLanguage -> Word32 -> OpSource repr
  opSourceExtension :: String -> OpSourceExtension repr
  -- opSpecConstant
  -- opSpecConstantComposite
  -- opSpecConstantFalse
  -- opSpecConstantTrue
  -- opSRem
  -- opStore
  opString :: String -> Result -> OpString repr
  -- opSwitch
  -- opTextureFetchBuffer
  -- opTextureFetchSample
  -- opTextureFetchTexel
  -- opTextureFetchTexelOffset
  -- opTextureGather
  -- opTextureGatherOffset
  -- opTextureGatherOffsets
  -- opTextureQueryLevels
  -- opTextureQueryLod
  -- opTextureQuerySamples
  -- opTextureQuerySize
  -- opTextureQuerySizeLod
  -- opTextureSample
  -- opTextureSampleDref
  -- opTextureSampleGrad
  -- opTextureSampleGradOffset
  -- opTextureSampleLod
  -- opTextureSampleLodOffset
  -- opTextureSampleOffset
  -- opTextureSampleProj
  -- opTextureSampleProjGrad
  -- opTextureSampleProjGradOffset
  -- opTextureSampleProjLod
  -- opTextureSampleProjLodOffset
  -- opTextureSampleProjOffset
  -- opTranspose
  -- opTypeArray
  opTypeBool :: ID -> OpTypeBool repr
  opTypeDeviceEvent :: ID -> OpTypeDeviceEvent repr
  opTypeEvent :: ID -> OpTypeEvent repr
  -- opTypeFilter
  opTypeFloat :: ID -> Word32 -> OpTypeFloat repr
  opTypeFunction :: ID -> TypeID -> [TypeID] -> OpTypeFunction repr
  opTypeInt :: ID -> Word32 -> Signedness -> OpTypeInt repr
  -- opTypeMatrix
  -- opTypeOpaque
  opTypePipe :: ID -> ID -> AccessQualifier -> OpTypePipe repr
  opTypePointer :: ID -> StorageClass -> ID -> OpTypePointer repr
  opTypeQueue :: ID -> OpTypeQueue repr
  opTypeReserveId :: ID -> OpTypeReserveId repr
  -- opTypeRuntimeArray
  -- opTypeSampler
  -- opTypeStruct
  -- opTypeVector :: ID ->
  opTypeVoid :: ID -> OpTypeVoid repr
  -- opUConvert
  -- opUDiv
  -- opUGreaterThan
  -- opUGreaterThanEqual
  -- opULessThan
  -- opULessThanEqual
  -- opUMod
  opUndef :: Result -> Result -> OpUndef repr
  -- opUnordered
  -- opUnreachable
  opVariable :: ID -> ID -> StorageClass -> Maybe ID -> OpVariable repr
  -- opVariableArray
  -- opVectorExtractDynamic
  -- opVectorInsertDynamic
  -- opVectorShuffle
  -- opVectorTimesMatrix
  -- opVectorTimesScalar
  -- opWaitGroupEvents
  -- opWritePipe

wordCount :: (Integral a) => String -> a
-- +1 because of stupid zero termination
-- 4 chars per Word32
wordCount = ceiling . (/4) . fromIntegral . (+1) . length

instance Instruction Instr where
  opNop = liftRepr $ Instr (WordCount 1) (OpCode $ ID 0) Nothing Nothing []
  -- opUndef (Result _) _ = Instr (WordCount 3) (OpCode $ ID 1) Nothing Nothing []
  -- opUndef r@(ResultType _) o = Instr (WordCount 3) (OpCode $ ID 1) (Just r) Nothing [ResultOperand o]
  opSource s ver = liftRepr $ SrcInstr (WordCount 3) (OpCode $ ID 3) s (LiteralW ver)
  opSourceExtension s = liftRepr $ Instr (WordCount $ 1 + wordCount s) (OpCode $ ID 4) Nothing Nothing [LiteralS s]
  opName n (ID rid) = liftRepr $ Instr (WordCount $ 2 + wordCount n) (OpCode $ ID 5) (Just $ Result $ ID rid) Nothing [LiteralS n]
  -- opMemberName n t = liftRepr $ Instr (WordCount $ 3 + wordCount n) (OpCode $ ID 6) (Just $ ResultType t) Nothing [LiteralI 0, LiteralS n]
  opString n rid@(Result (ID _)) = liftRepr $ Instr (WordCount $ 2 + wordCount n) (OpCode $ ID 7) (Just rid) Nothing [LiteralS n]
  opLine line col fid rid@(Result (ID _)) = liftRepr $ Instr (WordCount 5) (OpCode $ ID 8) (Just rid) Nothing [LiteralI $ fromIntegral line, LiteralI $ fromIntegral col]
  opExtension e = liftRepr $ Instr (WordCount $ 1 + wordCount e) (OpCode $ ID 10) Nothing Nothing [LiteralS e]
  opExtInstImport rid@(Result (ID _)) s = liftRepr $ Instr (WordCount $ 2 + wordCount s) (OpCode $ ID 11) (Just rid) Nothing [LiteralS s]
  --opMemoryModel :: AddressingModel -> MemoryModel -> repr
  opMemoryModel am mm = liftRepr $ Instr (WordCount 3) (OpCode $ ID 14) Nothing Nothing [LiteralW $ fromIntegral $ fromEnum am, LiteralW $ fromIntegral $ fromEnum mm]
  -- opEntryPoint :: ExecutionModel -> String -> repr
  opEntryPoint em rid@(Result (ID _)) s = liftRepr $ Instr (WordCount $ 3 + wordCount s) (OpCode $ ID 15) (Just rid) Nothing [LiteralW $ fromIntegral $ fromEnum em, LiteralS s]
  opTypeVoid rid = liftRepr $ Instr (WordCount 2) (OpCode $ ID 19) (Just $ Result rid) Nothing []
  opTypeBool rid = liftRepr $ Instr (WordCount 2) (OpCode $ ID 20) (Just $ Result rid) Nothing []
  --opTypeInt :: ID -> Word32 -> Signedness -> repr
  opTypeInt rid width signed = liftRepr $ Instr (WordCount 4) (OpCode $ ID 21) (Just $ Result rid) Nothing [LiteralW width, LiteralW $ fromIntegral $ fromEnum signed]
  opTypeFloat rid width = liftRepr $ Instr (WordCount 3) (OpCode $ ID 22) (Just $ Result rid) Nothing [LiteralW width]
  opTypeFunction rid rtype ptypes = liftRepr $ Instr (WordCount (3 + fromIntegral (length ptypes))) (OpCode $ ID 33) (Just $ Result rid) (Just rtype)
                                  $ ParamType <$> ptypes
  opFunction (TypeID rtid) (ID rid) fc (ID ftid) = liftRepr $ Instr (WordCount 5) (OpCode $ ID 54) (Just $ Result $ ID rtid) (Just $ TypeID rid) [LiteralW $ fromIntegral $ fromEnum fc, LiteralW ftid]
  opFunctionParameter rtid (ID rid) = liftRepr $ Instr (WordCount 3) (OpCode $ ID 55) (Just $ Result rtid) (Just $ TypeID rid) []
  opFunctionEnd = liftRepr $ Instr (WordCount 1) (OpCode $ ID 56) Nothing Nothing []
  opLabel rid = liftRepr $ Instr (WordCount 2) (OpCode $ ID 248) (Just $ Result rid) Nothing []
  opBranch rid = liftRepr $ Instr (WordCount 2) (OpCode $ ID 249) (Just $ Result rid) Nothing []
  opReturn = liftRepr $ Instr (WordCount 1) (OpCode $ ID 253) Nothing Nothing []
  -- TODO: how to encode the operands here? specs are unclear
  opVariable (ID rtid) rid sclass initid = liftRepr $ Instr (WordCount 4) (OpCode $ ID 59) (Just $ Result rid) (Just $ TypeID rtid) []
  -- opTypePointer :: ID -> StorageClass -> ID -> repr
  opTypePointer rid sclass (ID tid) = liftRepr $ Instr (WordCount 4) (OpCode $ ID 32) (Just $ Result rid) Nothing [LiteralW $ fromIntegral $ fromEnum sclass, Type $ TypeID tid]
  -- opTypeEvent :: ID -> repr
  opTypeEvent rid = liftRepr $ Instr (WordCount 2) (OpCode $ ID 34) (Just $ Result rid) Nothing []
  opTypeDeviceEvent rid = liftRepr $ Instr (WordCount 2) (OpCode $ ID 35) (Just $ Result rid) Nothing []
  opTypeReserveId rid = liftRepr $ Instr (WordCount 2) (OpCode $ ID 36) (Just $ Result rid) Nothing []
  opTypeQueue rid = liftRepr $ Instr (WordCount 2) (OpCode $ ID 37) (Just $ Result rid) Nothing []
  -- opTypePipe :: ID -> ID -> AccessQualifier -> repr
  opTypePipe rid (ID tid) acc = liftRepr $ Instr (WordCount 4) (OpCode $ ID 38) (Just $ Result rid) (Just $ TypeID tid) [LiteralW $ fromIntegral $ fromEnum acc]
  -- opConstantTrue  :: ID -> ID -> repr
  -- opConstantTrue rtid rid = liftRepr $ Instr (WordCount 4) (OpCode $ ID 41) (Just $ ResultType rtid) Nothing [ResultOperand rid]


class Word32VectEncode t where
  encodeWord32Vect :: t -> V.Vector Word32

instance Word32VectEncode TypeOp where
  encodeWord32Vect (BoolType (OpCode (ID tid))) = V.singleton tid
  encodeWord32Vect (IntType (OpCode (ID tid))) = V.singleton tid

instance Word32VectEncode Result where
  encodeWord32Vect (Result (ID rid)) = V.singleton $ fromIntegral rid

instance Word32VectEncode TypeID where
  encodeWord32Vect (TypeID t) = V.singleton t

encodeWord32VectImplOrd :: [Word8] -> [Word32]
encodeWord32VectImplOrd (a:b:c:d:rest) =
  fromHiAndLo (fromHiAndLo d c) (fromHiAndLo b a) : encodeWord32VectImplOrd rest
encodeWord32VectImplOrd (a:b:c:rest) =
  fromHiAndLo (fromHiAndLo 0 c) (fromHiAndLo b a) : encodeWord32VectImplOrd rest
encodeWord32VectImplOrd (a:b:rest) =
  fromHiAndLo 0 (fromHiAndLo b a) : encodeWord32VectImplOrd rest
encodeWord32VectImplOrd (a:rest) =
  fromHiAndLo 0 (fromHiAndLo 0 a) : encodeWord32VectImplOrd rest
encodeWord32VectImplOrd _ = mempty

encodeWord32VectImpl :: String -> V.Vector Word32
encodeWord32VectImpl = V.fromList . encodeWord32VectImplOrd . fmap (fromIntegral . ord)

instance Word32VectEncode String where
  -- | String must be zero terminated. Don't we all love C.
  encodeWord32Vect s = encodeWord32VectImpl $ s ++ [chr 0]

paddFalseExtend :: Int -> V.Vector Bool -> V.Vector Bool
paddFalseExtend fsize bools = bools V.++ V.replicate missing False
  where missing = fsize - (V.length bools `mod` fsize)

setBitFunc :: (Bits a) => Int -> Bool -> a -> a
setBitFunc _ False val = val
setBitFunc ix True val = setBit val ix

genBitted :: (Bits a) => V.Vector Bool -> a
genBitted = V.ifoldr setBitFunc zeroBits


generateChunkedBitsVect :: forall a. (FiniteBits a, Bits a) => V.Vector Bool -> V.Vector a
generateChunkedBitsVect bools =
  V.map genBitted $ V.fromList $ fmap (sliceV bs paddedBools) starts
  where
    bs = finiteBitSize (undefined :: a)
    paddedBools = paddFalseExtend bs bools
    elemsCount = V.length paddedBools `div` bs
    starts = [ x * bs | x <- [0..elemsCount-1]]
    sliceV len v start = V.slice start len v

encodeIntegerWord32Vect :: (FiniteBits a, Bits a) => Integer -> V.Vector a
encodeIntegerWord32Vect i
  | totalBits == 0
  = mempty
  | otherwise
  = generateChunkedBitsVect allBits
  where
    totalBits = popCount i
    allBits = V.fromList $ testBit i <$> [0..totalBits]

instance Word32VectEncode a => Word32VectEncode (Maybe a) where
  encodeWord32Vect = maybe mempty encodeWord32Vect

instance Word32VectEncode a => Word32VectEncode [a] where
  encodeWord32Vect = V.concat . fmap encodeWord32Vect

instance Word32VectEncode Integer where
  encodeWord32Vect = encodeIntegerWord32Vect

instance Word32VectEncode Operand where
  encodeWord32Vect (Operand o) = V.singleton o
  encodeWord32Vect (LiteralS s) = encodeWord32Vect s
  encodeWord32Vect (LiteralI i) = encodeWord32Vect i
  encodeWord32Vect (LiteralW i) = V.singleton i
  encodeWord32Vect (Type (TypeID t)) = V.singleton t
  encodeWord32Vect (ParamType (TypeID t)) = V.singleton t
  encodeWord32Vect (ResultOperand r) = encodeWord32Vect r

instance Word32VectEncode SourceLanguage where
  encodeWord32Vect = V.singleton . fromIntegral . fromEnum

instance Word32VectEncode ExecutionModel where
  encodeWord32Vect = V.singleton . fromIntegral . fromEnum

instance Word32VectEncode ExecutionMode where
  encodeWord32Vect = V.singleton . fromIntegral . fromEnum

instance Word32VectEncode AddressingModel where
  encodeWord32Vect = V.singleton . fromIntegral . fromEnum

instance Word32VectEncode Signedness where
  encodeWord32Vect = V.singleton . fromIntegral . fromEnum

instance Word32VectEncode StorageClass where
  encodeWord32Vect = V.singleton . fromIntegral . fromEnum

instance Word32VectEncode AccessQualifier where
  encodeWord32Vect = V.singleton . fromIntegral . fromEnum

instance Word32VectEncode Instr where
  encodeWord32Vect (Instr (WordCount count) (OpCode (ID oid)) result typeId operands) =
    V.concat $
           [ V.singleton $ fromHiAndLo (fromIntegral count) (fromIntegral oid)
           , encodeWord32Vect typeId
           , encodeWord32Vect result
           ]
           ++
           operandsVect
    where
      operandsVect = fmap encodeWord32Vect operands
  encodeWord32Vect (SrcInstr (WordCount count) (OpCode (ID oid)) lang op) =
    V.concat $
           [ V.singleton $ fromHiAndLo (fromIntegral count) (fromIntegral oid)
           , encodeWord32Vect lang
           , encodeWord32Vect op
           ]

instance {-# OVERLAPPING #-} (OpCodeClass a repr, Word32VectEncode repr) => Word32VectEncode (a repr) where
  encodeWord32Vect = encodeWord32Vect . unliftRepr

version :: Word32
version = 99

thisGeneratorMagic :: Word32
-- thisGeneratorMagic = 1882
thisGeneratorMagic = 85590203

instructionSchema :: Word32
instructionSchema = 0

moduleHeader :: Int -> V.Vector Word32
moduleHeader idlimit =
  V.fromList [ magic
             , version
             , thisGeneratorMagic
             , fromIntegral idlimit
             , instructionSchema
             ]

fullModuleWords :: [Instr] -> V.Vector Word32
fullModuleWords instr = moduleHeader 0 V.++ V.concat (fmap encodeWord32Vect instr)

data DebugOps repr = DebugOps [OpString repr] [OpName repr] [OpMemberName repr] [OpLine repr]
data FuncStartNoBody repr = FuncStartNoBody (OpFunction repr)
data FuncConsNoBody repr = FuncConsNoBody (OpFunction repr) [OpFunctionParameter repr]
data FuncNoBody repr = FuncNoBody (OpFunction repr) [OpFunctionParameter repr] (OpFunctionEnd repr)

instance (Instruction repr,  Word32VectEncode repr) => Word32VectEncode (FuncNoBody repr) where
  encodeWord32Vect (FuncNoBody opf opfps end) = V.concat [encodeWord32Vect opf, encodeWord32Vect opfps, encodeWord32Vect end]

startFuncNoBody :: OpFunction repr -> FuncStartNoBody repr
startFuncNoBody = FuncStartNoBody

paramFirstNoBody :: FuncStartNoBody repr -> OpFunctionParameter repr -> FuncConsNoBody repr
paramFirstNoBody (FuncStartNoBody opf) opfp = FuncConsNoBody opf [opfp]

paramNextNoBody :: FuncConsNoBody repr -> OpFunctionParameter repr -> FuncConsNoBody repr
paramNextNoBody (FuncConsNoBody opf opfps) opfp = FuncConsNoBody opf $ opfps ++ [opfp]

endFuncNoBody :: (Instruction repr) => FuncConsNoBody repr -> FuncNoBody repr
endFuncNoBody (FuncConsNoBody opf opfps) = FuncNoBody opf opfps opFunctionEnd

data FuncStart repr = FuncStart (OpFunction repr)
data FuncParamCons repr = FuncParamCons (OpFunction repr) [OpFunctionParameter repr]
data FuncParamEnd repr = FuncParamEnd (OpFunction repr) [OpFunctionParameter repr]
data FuncParamsBlockCons repr = FuncParamsBlockCons (OpFunction repr) [OpFunctionParameter repr] (Blocks repr)
data FuncParamsBlockEnd repr = FuncParamsBlockEnd (OpFunction repr) [OpFunctionParameter repr] (Blocks repr)
data Func repr = Func (OpFunction repr) [OpFunctionParameter repr] (OpFunctionEnd repr)

instance (Word32VectEncode repr) => Word32VectEncode (Func repr) where
  encodeWord32Vect (Func opf opfps end) = V.concat [encodeWord32Vect opf, encodeWord32Vect opfps, encodeWord32Vect end]

startFunc :: OpFunction repr -> FuncStart repr
startFunc = FuncStart

paramFirst :: FuncStart repr -> OpFunctionParameter repr -> FuncParamCons repr
paramFirst (FuncStart opf) opfp = FuncParamCons opf [opfp]

paramNext :: FuncParamCons repr -> OpFunctionParameter repr -> FuncParamCons repr
paramNext (FuncParamCons opf opfps) opfp = FuncParamCons opf $ opfps ++ [opfp]

endFunc :: (Instruction repr) => FuncParamsBlockEnd repr -> Func repr
endFunc (FuncParamsBlockEnd opf opfps blocks) = Func opf opfps opFunctionEnd

data BlockStart repr = BlockStart (Maybe (OpLine repr)) (OpLabel repr)
data BlockFirst repr = BlockFirst (FuncParamsBlockEnd repr) (Maybe (OpLine repr)) (OpLabel repr)
data BlockFirstCont repr = BlockFirstCont (FuncParamsBlockEnd repr) (Maybe (OpLine repr)) (OpLabel repr) [OpVariable repr]
data BlockFirstEnd repr = BlockFirstEnd (FuncParamsBlockEnd repr) (Maybe (OpLine repr)) (OpLabel repr) [OpVariable repr] (OpBranch repr)
data BlockNext repr = BlockNext (FuncParamsBlockEnd repr) (Maybe (OpLine repr)) (OpLabel repr) [OpVariable repr] [OpBranch repr]
data Blocks repr = Blocks

firstBlock :: FuncParamsBlockEnd repr -> Maybe (OpLine repr) -> OpLabel repr -> BlockFirst repr
firstBlock = BlockFirst


-- | TODO: enforce storage class function
blockOpVariableFirst :: BlockFirst repr -> OpVariable repr -> BlockFirstCont repr
blockOpVariableFirst (BlockFirst funcblockEnd opline label) var = BlockFirstCont funcblockEnd opline label [var]

blockOpVariableNext :: BlockFirstCont repr -> OpVariable repr -> BlockFirstCont repr
blockOpVariableNext (BlockFirstCont funcblockEnd opline label vars) nvar = BlockFirstCont funcblockEnd opline label $ vars ++ [nvar]

endFirstBlock :: BlockFirstCont repr -> OpBranch repr -> BlockFirstEnd repr
endFirstBlock (BlockFirstCont funcblockEnd opline label vars) = BlockFirstEnd funcblockEnd opline label vars


data FullModule repr = FullModule
                  (Maybe (OpSource repr))
                  (Maybe (OpSourceExtension repr))
                  -- [OpCapability]
                  (Maybe [OpExtension repr])
                  (Maybe [OpExtInstImport repr])
                  (OpMemoryModel repr)
                  [OpEntryPoint repr]
                  [OpExecutionMode repr]
                  [FuncNoBody repr]
                  [Func repr]

instance (Instruction repr, Word32VectEncode repr) => Word32VectEncode (FullModule repr) where
  encodeWord32Vect (FullModule msrc msext mexts mextsimps opmm opep opem funcns funcs) =
    V.concat [ moduleHeader 0
             , encodeWord32Vect msrc
             , encodeWord32Vect msext
             , encodeWord32Vect mexts
             , encodeWord32Vect mextsimps
             , encodeWord32Vect opmm
             , encodeWord32Vect opep
             , encodeWord32Vect opem
             , encodeWord32Vect funcns
             , encodeWord32Vect funcs
             ]

fullModule :: Maybe (OpSource repr)
          ->  Maybe (OpSourceExtension repr)
          -- ->  [OpCapability]
          ->  Maybe [OpExtension repr]
          ->  Maybe [OpExtInstImport repr]
          ->  OpMemoryModel repr
          ->  [OpEntryPoint repr]
          ->  [OpExecutionMode repr]
          ->  [FuncNoBody repr]
          ->  [Func repr]
          ->  FullModule repr
fullModule = FullModule
