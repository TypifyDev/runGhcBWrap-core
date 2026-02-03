
liftPure1 :: FunctionName -> Fn
liftPure1 = liftFn . mkF1

liftPure2 :: FunctionName -> Fn
liftPure2 = liftFn . mkF2

liftPure3 :: FunctionName -> Fn
liftPure3 = liftFn . mkF3

liftPure4 :: FunctionName -> Fn
liftPure4 = liftFn . mkF4

liftPure5 :: FunctionName -> Fn
liftPure5 = liftFn . mkF5

liftPure6 :: FunctionName -> Fn
liftPure6 = liftFn . mkF6

liftPure7 :: FunctionName -> Fn
liftPure7 = liftFn . mkF7

liftPure8 :: FunctionName -> Fn
liftPure8 = liftFn . mkF8

liftPure9 :: FunctionName -> Fn
liftPure9 = liftFn . mkF9

liftPure10 :: FunctionName -> Fn
liftPure10 = liftFn . mkF10

liftPure11 :: FunctionName -> Fn
liftPure11 = liftFn . mkF11

liftPure12 :: FunctionName -> Fn
liftPure12 = liftFn . mkF12

liftPure13 :: FunctionName -> Fn
liftPure13 = liftFn . mkF13

liftPure14 :: FunctionName -> Fn
liftPure14 = liftFn . mkF14

liftPure15 :: FunctionName -> Fn
liftPure15 = liftFn . mkF15

liftPure16 :: FunctionName -> Fn
liftPure16 = liftFn . mkF16

liftPure17 :: FunctionName -> Fn
liftPure17 = liftFn . mkF17

liftPure18 :: FunctionName -> Fn
liftPure18 = liftFn . mkF18

liftPure19 :: FunctionName -> Fn
liftPure19 = liftFn . mkF19

liftPure20 :: FunctionName -> Fn
liftPure20 = liftFn . mkF20

-- No longer necesssary due to property of Monadic being encoded in the type
liftPureT1 :: FunctionName -> FnT a b
liftPureT1 = liftFnT . mkFT1

liftPureT2 :: FunctionName -> FnT (a,b) c
liftPureT2 = liftFnT . mkFT2

liftPureT3 :: FunctionName -> FnT (a,b,c) d
liftPureT3 = liftFnT . mkFT3

liftPureT4 :: FunctionName -> FnT (a,b,c,d) e
liftPureT4 = liftFnT . mkFT4

liftPureT5 :: FunctionName -> FnT (a,b,c,d,e) f
liftPureT5 = liftFnT . mkFT5

liftPureT6 :: FunctionName -> FnT (a,b,c,d,e,f) g
liftPureT6 = liftFnT . mkFT6

liftPureT7 :: FunctionName -> FnT (a,b,c,d,e,f,g) h
liftPureT7 = liftFnT . mkFT7

liftPureT8 :: FunctionName -> FnT (a,b,c,d,e,f,g,h) i
liftPureT8 = liftFnT . mkFT8

liftPureT9 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i) j
liftPureT9 = liftFnT . mkFT9

liftPureT10 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j) k
liftPureT10 = liftFnT . mkFT10

liftPureT11 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k) l
liftPureT11 = liftFnT . mkFT11

liftPureT12 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l) m
liftPureT12 = liftFnT . mkFT12

liftPureT13 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m) n
liftPureT13 = liftFnT . mkFT13

liftPureT14 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n) o
liftPureT14 = liftFnT . mkFT14

liftPureT15 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) p
liftPureT15 = liftFnT . mkFT15

liftPureT16 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) q
liftPureT16 = liftFnT . mkFT16

liftPureT17 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) r
liftPureT17 = liftFnT . mkFT17

liftPureT18 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) s
liftPureT18 = liftFnT . mkFT18

liftPureT19 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) t
liftPureT19 = liftFnT . mkFT19

liftPureT20 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) u
liftPureT20 = liftFnT . mkFT20


mkFHKT1 :: Purity -> FunctionName -> FnHKT a b
mkFHKT1 purity fname = FnHKT purity . pack $ [istr| \a -> #{fname} a|] 

mkFHKT2 :: Purity -> FunctionName -> FnHKT a b
mkFHKT2 purity fname = FnHKT purity . pack $ [istr| \(a, b) -> #{fname} a b|] 

mkFHKT3 :: Purity -> FunctionName -> FnHKT a b
mkFHKT3 purity fname = FnHKT purity . pack $ [istr| \(a, b, c) -> #{fname} a b c|] 

mkFHKT4 :: Purity -> FunctionName -> FnHKT a b
mkFHKT4 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d) -> #{fname} a b c d|] 

mkFHKT5 :: Purity -> FunctionName -> FnHKT a b
mkFHKT5 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e) -> #{fname} a b c d e|] 

mkFHKT6 :: Purity -> FunctionName -> FnHKT a b
mkFHKT6 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f) -> #{fname} a b c d e f|] 

mkFHKT7 :: Purity -> FunctionName -> FnHKT a b
mkFHKT7 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g) -> #{fname} a b c d e f g|] 

mkFHKT8 :: Purity -> FunctionName -> FnHKT a b
mkFHKT8 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h) -> #{fname} a b c d e f g h|] 

mkFHKT9 :: Purity -> FunctionName -> FnHKT a b
mkFHKT9 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i) -> #{fname} a b c d e f g h i|] 

mkFHKT10 :: Purity -> FunctionName -> FnHKT a b
mkFHKT10 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j) -> #{fname} a b c d e f g h i j|] 

mkFHKT11 :: Purity -> FunctionName -> FnHKT a b
mkFHKT11 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k) -> #{fname} a b c d e f g h i j k|] 

mkFHKT12 :: Purity -> FunctionName -> FnHKT a b
mkFHKT12 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l) -> #{fname} a b c d e f g h i j k l|] 

mkFHKT13 :: Purity -> FunctionName -> FnHKT a b
mkFHKT13 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m) -> #{fname} a b c d e f g h i j k l m|] 

mkFHKT14 :: Purity -> FunctionName -> FnHKT a b
mkFHKT14 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> #{fname} a b c d e f g h i j k l m n|] 

mkFHKT15 :: Purity -> FunctionName -> FnHKT a b
mkFHKT15 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> #{fname} a b c d e f g h i j k l m n o|] 

mkFHKT16 :: Purity -> FunctionName -> FnHKT a b
mkFHKT16 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> #{fname} a b c d e f g h i j k l m n o p|] 

mkFHKT17 :: Purity -> FunctionName -> FnHKT a b
mkFHKT17 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> #{fname} a b c d e f g h i j k l m n o p q|] 

mkFHKT18 :: Purity -> FunctionName -> FnHKT a b
mkFHKT18 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> #{fname} a b c d e f g h i j k l m n o p q r|] 

mkFHKT19 :: Purity -> FunctionName -> FnHKT a b
mkFHKT19 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> #{fname} a b c d e f g h i j k l m n o p q r s|] 

mkFHKT20 :: Purity -> FunctionName -> FnHKT a b
mkFHKT20 purity fname = FnHKT purity . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> #{fname} a b c d e f g h i j k l m n o p q r s t|] 


compareArityT1
  :: Purity -> NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareArityT1 purity nnScript srcCode = compareFuncHKT purity nnScript srcCode
  

comparePureArity1 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity1 = compareFunc liftPure1

comparePureArity2 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity2 = compareFunc liftPure2

comparePureArity3 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity3 = compareFunc liftPure3

comparePureArity4 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity4 = compareFunc liftPure4

comparePureArity5 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity5 = compareFunc liftPure5

comparePureArity6 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity6 = compareFunc liftPure6

comparePureArity7 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity7 = compareFunc liftPure7

comparePureArity8 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity8 = compareFunc liftPure8

comparePureArity9 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity9 = compareFunc liftPure9

comparePureArity10 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity10 = compareFunc liftPure10

comparePureArity11 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity11 = compareFunc liftPure11

comparePureArity12 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity12 = compareFunc liftPure12

comparePureArity13 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity13 = compareFunc liftPure13

comparePureArity14 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity14 = compareFunc liftPure14

comparePureArity15 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity15 = compareFunc liftPure15

comparePureArity16 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity16 = compareFunc liftPure16

comparePureArity17 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity17 = compareFunc liftPure17

comparePureArity18 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity18 = compareFunc liftPure18

comparePureArity19 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity19 = compareFunc liftPure19

comparePureArity20 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity20 = compareFunc liftPure20


-- Monadic Actions of Some Arity

compareMonadicArity1 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity1 = compareFunc mkF1

compareMonadicArity2 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity2 = compareFunc mkF2

compareMonadicArity3 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity3 = compareFunc mkF3

compareMonadicArity4 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity4 = compareFunc mkF4

compareMonadicArity5 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity5 = compareFunc mkF5

compareMonadicArity6 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity6 = compareFunc mkF6

compareMonadicArity7 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity7 = compareFunc mkF7

compareMonadicArity8 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity8 = compareFunc mkF8

compareMonadicArity9 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity9 = compareFunc mkF9

compareMonadicArity10 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity10 = compareFunc mkF10

compareMonadicArity11 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity11 = compareFunc mkF11

compareMonadicArity12 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity12 = compareFunc mkF12

compareMonadicArity13 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity13 = compareFunc mkF13

compareMonadicArity14 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity14 = compareFunc mkF14

compareMonadicArity15 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity15 = compareFunc mkF15

compareMonadicArity16 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity16 = compareFunc mkF16

compareMonadicArity17 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity17 = compareFunc mkF17

compareMonadicArity18 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity18 = compareFunc mkF18

compareMonadicArity19 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity19 = compareFunc mkF19

compareMonadicArity20 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity20 = compareFunc mkF20
