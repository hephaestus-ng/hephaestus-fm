module Parser.Grammar where


data FGroup = AltFeature String
            | OrFeature String
            | BasicFeature String

data FType = Mandatory String
           | Optional String


data Feature = Feature {
  group :: FGroup,
  type  :: FType
}


data Expr = Var String
          | Imply Expr Expr
          | 
