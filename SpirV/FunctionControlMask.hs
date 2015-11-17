module SpirV.FunctionControlMask (FunctionControlMask(..)) where
data FunctionControlMask
  = InLine
  | DontInline
  | Pure
  | Const

instance Enum FunctionControlMask where
  toEnum i = case i of
                0x0 -> InLine
                0x1 -> DontInline
                0x4 -> Pure
                0x8 -> Const
                _ -> error $ "Invalid enum int " ++ show i ++ " for FunctionControlMask"
  fromEnum InLine = 0x0
  fromEnum DontInline = 0x2
  fromEnum Pure = 0x4
  fromEnum Const = 0x8
