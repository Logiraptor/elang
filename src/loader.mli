
module Make(L : Language.FileResolver)(P : Language.Parser) : Language.Loader with type module_ = P.ast
