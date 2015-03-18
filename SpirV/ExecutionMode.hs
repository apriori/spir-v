module SpirV.ExecutionMode (ExecutionMode(..)) where
data ExecutionMode
  = Invocations 
  | SpacingEqual 
  | SpacingFractionalEven 
  | SpacingFractionalOdd 
  | VertexOrderCw 
  | VertexOrderCcw 
  | PixelCenterInteger 
  | OriginUpperLeft 
  | EarlyFragmentTests 
  | PointMode 
  | Xfb 
  | DepthReplacing 
  | DepthAny 
  | DepthGreater 
  | DepthLess 
  | DepthUnchanged 
  | LocalSize 
  | LocalSizeHint 
  | InputPoints 
  | InputLines 
  | InputLinesAdjacency 
  | InputTriangles 
  | InputTrianglesAdjacency 
  | InputQuads 
  | InputIsolines 
  | OutputVertices 
  | OutputPoints 
  | OutputLineStrip 
  | OutputTriangleStrip 
  | VecTypeHint 
  | ContractionOff 