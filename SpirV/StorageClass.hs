module SpirV.StorageClass (StorageClass(..)) where
data StorageClass
  = Input
  | Uniform
  | Output
  | WorkgroupLocal
  | WorkgroupGlobal
  | PrivateGlobal
  | Function
  | Generic
  | AtomicCounter
  | Image
  deriving (Enum)
