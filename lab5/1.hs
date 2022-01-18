-- getLine >>= \l1 -> return (l1++l1) >>= \l2 -> print [l1,l2]

myFunc = do
    l1 <- getLine 
    l2 <- return (l1 ++ l1)
    print [l1,l2]