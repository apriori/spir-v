module SpirV.MemoryModel (MemoryModel(..)) where
data MemoryModel
  = Simple
  | GLSL450
  | OpenCL
  deriving (Enum)
