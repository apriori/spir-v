module SpirV.FunctionParameterAttribute
       (FunctionParameterAttribute(..)) where
data FunctionParameterAttribute
  = Zext 
  | Sext 
  | ByVal 
  | Sret 
  | NoAlias 
  | NoCapture 
  | SVM 
  | NoWrite 
  | NoReadWrite 