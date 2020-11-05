import Data.List
import Data.Char
import System.IO


--Small lambdas used in code
isWhiteSpace = (\c -> c `elem` " \t")
isUnnecessery = (\c -> c `elem` "{}, ")
isOperator = (\c -> c `elem` "+-/*")
getProdByLetter = ((\prod a ->  head $ filter (matchLetter a) prod))
getProdLHS = (\prod a -> head $ snd $ getProdByLetter prod a)
isUnaryMinus = (\c -> c == '-')

-- Used in 'getProdByLetter' lambda
matchLetter :: Char -> (Char, String) -> Bool
matchLetter a x = (fst x) ==  a

-- Printing all results (except for graph) to the file
writeInfoToFile deptSet inDeptSet fnf graphDerivedFnf name = do
    handle <- openFile name WriteMode
    hPutStrLn handle "D ="
    hPrint handle deptSet
    hPutStrLn handle "I ="
    hPrint handle inDeptSet
    hPutStrLn handle "FNF ="
    hPrint handle fnf
    hPutStrLn handle "Graph-derived FNF ="
    hPrint handle graphDerivedFnf
    hClose handle

-- Graph file priting (Dot Format)
writeDotGraphToFile graph name = do
    handle <- openFile name WriteMode
    hPutStrLn handle "digraph g {"
    mapM_ (hPutStrLn handle) [show s ++ " -> " ++ (show t)| (s,t) <- (fst graph)]
    mapM_ (hPutStrLn handle) [show no ++ "[label=" ++ [l] ++ "]" | (no, l) <- (snd graph)]
    hPutStrLn handle "}"
    hClose handle

-- Graph stdout priting (Dot Format)
printGraphInDotFormat graph = do
    putStrLn "digraph g {"
    mapM_ putStrLn edges'
    mapM_ putStrLn vertexLabels'
    putStrLn "}"
    where
        edges = fst graph
        vertexLabels = snd graph
        edges' = [show s ++ " -> " ++ (show t)| (s,t) <- edges]
        vertexLabels' = [show no ++ "[label=" ++ [l] ++ "]" | (no, l) <- vertexLabels ]

-- Build FNF based on minimized graph
fnfDerivedFromMinimizedGraph :: ([(Int, Int)], [(Int, Char)]) -> String
fnfDerivedFromMinimizedGraph graph =
    if (snd graph) == []
        then ""
        else "(" ++ res ++ ")" ++ fnfDerivedFromMinimizedGraph graph'
        where
            vertexLabels = snd graph
            edges = fst graph
            res = [l | (no, l) <- vertexLabels, not $ no `elem` (snd $ unzip edges)]
            res' = [no | (no, l) <- vertexLabels, not $ no `elem` (snd $ unzip edges)]
            graph' = (edges', vertexLabels')
            vertexLabels' = [(no, l) | (no, l) <- vertexLabels, not $ no `elem` res']
            edges' = [(s, t) | (s, t) <- edges, not $ s `elem` res']



-- Build Dot graph
------------------------

-- Checks recursively if there is an indirect path from 'primeS' to 't' (at start 's' == 'primeS')
findIndirectPath :: [(Int, Int)] -> Int -> Int -> Int -> Bool
findIndirectPath edges s t primeS =
    if edges == [] || s >= t
        then False
        else if s /= primeS && (s, t) `elem` edges
            then True
            else answer
            where
                answer = foldl (||) False [findIndirectPath edges (snd s') t primeS | s' <- edges, s == (fst s')]

-- Minimizes initial graph by leaving only necessery edges (if there is an indirect path from s to t then edge (s,t) can be deleted)
minimizeEdgeCount :: ([(Int, Int)], [(Int, Char)]) -> ([(Int, Int)], [(Int, Char)])
minimizeEdgeCount graph = (edges', vertexLabels)
    where
        vertexLabels = snd graph
        edges = fst graph
        edges' = [(s, t) | (s, t) <- edges,  not $ findIndirectPath edges s t s]

-- Recursively build initial graph by creating edges to connected dependent nodes (letters) that follow each letter in word w
recurGraphBuilding :: [(Char, Char)] -> String -> ([(Int, Int)], [(Int, Char)]) -> Int -> ([(Int, Int)], [(Int, Char)])
recurGraphBuilding deptSet w graph i  =
    if w == ""
        then graph
        else recurGraphBuilding deptSet (tail w) graph' (i+1)
        where 
            edges = fst graph
            vertexLabels = snd graph
            graph' = (edges', vertexLabels)
            l = head w
            edges' = edges ++ [(i, fst v) | v <- vertexLabels, i < (fst v), (l, (snd v)) `elem` deptSet]

-- Creates initial graph (with unnecessery edges)
createInitialDotGraph :: [(Char, Char)] -> String -> ([(Int, Int)], [(Int, Char)])
createInitialDotGraph deptSet w = recurGraphBuilding deptSet w ([], vertexLabels) 1
    where
        vertexLabels = zip [1..(length w)] w

-- Function creating minimized graph in Dot format
createDotGraph :: [(Char, Char)] -> String -> ([(Int, Int)], [(Int, Char)])
createDotGraph deptSet w = minimizeEdgeCount $ createInitialDotGraph deptSet w

----------------------------
-- Building Dot Graph



-- FNF based on set I and word w
----------------------------------------
fnf :: [(Char, Char)] -> String -> String -> String
fnf inDeptSet w flNormForm = case w of
    ""  -> flNormForm
    _   -> fnf inDeptSet w' flNormForm'
    where
        fnfSegment = recurIndependency inDeptSet "" w 0
        flNormForm' = flNormForm ++ "(" ++ fnfSegment ++ ")"
        w' = deleteFromWord fnfSegment w

-- Deletes letters that were added to FNF in the latest segment
deleteFromWord :: String -> String -> String
deleteFromWord fnfSegment w = case fnfSegment of
    ""  -> w
    _   -> deleteFromWord (tail fnfSegment) w'
    where   
        (prev, next) = break ((head fnfSegment) ==) w
        w' = if  prev == ""
            then tail next
            else prev ++ (tail next)

-- Checks if letter 'letter' is independent from all others in FNF segment that is currently being created
letterIsIndeptWithRest :: [(Char, Char)] -> Char -> String -> Bool
letterIsIndeptWithRest inDeptSet letter word' = 
    if (length [l | l <- word', not $ (letter, l) `elem` inDeptSet]) == 0
        then True
        else False

-- Creates FNF segment recurisvely by checking if there are letters that are independent from all other in FNF segment that it creates
recurIndependency :: [(Char, Char)] -> String -> String -> Int -> String
recurIndependency inDeptSet fnfSegment w i =
    if i == (length w)
        then fnfSegment
        else if letterIsIndeptWithRest inDeptSet (w !! i) (take i w)
            then recurIndependency inDeptSet (fnfSegment ++ [(w !! i)]) w (i+1)
            else recurIndependency inDeptSet fnfSegment w (i+1)

----------------------------------------
-- Useful function helping calculate FNF


-- Calculate set D and I
------------------------
-- Calculate D
calculateDependencySet :: [(Char, String)] -> String -> [(Char, Char)]
calculateDependencySet productions alphabet = 
    [(x, y) | x <- alphabet, y <- alphabet, x == y || containsVarOnProdRHS productions y (getProdLHS productions x) || containsVarOnProdRHS productions x (getProdLHS productions y) ]

-- Calculate I
calculateIndependencySet :: [(Char, String)] -> String -> [(Char, Char)]
calculateIndependencySet productions alphabet = [(x, y) | x <- alphabet, y <- alphabet, not ((x, y) `elem` d)] where d = calculateDependencySet productions alphabet

-- Get left hand side of the production related to letter 'prodLetter' from alphabet A
getProdRHSVars :: [(Char, String)] -> Char -> String
getProdRHSVars productions prodLetter = 
    let (_, res) = splitAt 1 $ snd $ (getProdByLetter productions prodLetter)
    in res

-- Check if variable 'var' appears on the right hand side of the production relateed to letter 'prodLetter' from alphabet A
containsVarOnProdRHS :: [(Char, String)] -> Char -> Char -> Bool
containsVarOnProdRHS productions prodLetter var = 
    var `elem` (getProdRHSVars productions prodLetter)
------------------------



-- DANGER ZONE
----------------------------------------
-- Parsing input (done manually)
deleteDigitsFromProd :: String -> String
deleteDigitsFromProd prod = filter (not . isDigit) prod

nextOperandNextPhase :: String -> String
nextOperandNextPhase prod =
    if prod == ""
        then ""
        else if isWhiteSpace (head prod)
            then dropWhile isWhiteSpace prod
            else prod


nextOperand :: String -> String
nextOperand prod = 
    if isWhiteSpace (head prod)
        then nextOperandNextPhase prod'
        else nextOperandNextPhase (tail prod)
        where (_, prod') = splitAt 1 (dropWhile isWhiteSpace prod)

parseProductionPhase4 :: String -> String -> String -> (String, String)
parseProductionPhase4 prod letters prodVars =
    if prod == ""
        then (letters, prodVars)
        else if prod' == ""
            then (letters, prodVars)
            else parseProductionPhase3 prod' letters prodVars
            where prod' = nextOperand prod


parseProductionPhase3 ::String -> String -> String -> (String, String)
parseProductionPhase3 prod letters prodVars = 
    if isUnaryMinus (head prod)
        then (parseProductionPhase4 prod'' letters (prodVars ++ [(prod !! 1)]))
        else (parseProductionPhase4 prod' letters (prodVars ++ [(head prod)])) 
        where 
            (_, prod') = splitAt 1 prod
            (_, prod'') = splitAt 2 prod


parseProductionPhase2 :: String -> String -> String -> (String, String)
parseProductionPhase2 prod letters prodVars =
    if (take 2 prod) /= ":="
        then (letters, prod)
        else if isWhiteSpace (prod !! 2) 
            then parseProductionPhase3 (dropWhile isWhiteSpace (drop 2 prod)) letters prodVars
            else parseProductionPhase3 (drop 2 prod) letters prodVars

parseProductionPhase1 :: String -> String -> String -> (String, String)
parseProductionPhase1 prod letters prodVars = 
    if isWhiteSpace (prod !! 1)
        then parseProductionPhase2 (dropWhile isWhiteSpace (tail prod)) letters ([(head prod)] ++ prodVars)
        else parseProductionPhase2 (tail prod) letters ([(head prod)] ++ prodVars)

parseProductionPrePhase :: String -> String -> String -> (String, String)
parseProductionPrePhase prod letters prodVars =
    if (head prod) == '('
        then parseProductionPhase1 prod' letters' prodVars
        else parseProductionPhase1 prod letters prodVars
        where
            letters' = letters ++ [(prod !! 1)] 
            prod' = if isWhiteSpace (prod !! 3)
                then dropWhile isWhiteSpace (drop 3 prod)
                else drop 3 prod

parseProduction :: String -> String -> String -> (String, String)
parseProduction prod letters prodVars = 
    if isWhiteSpace (head prod)
        then parseProductionPrePhase (dropWhile isWhiteSpace prod) letters prodVars
        else parseProductionPrePhase prod letters prodVars


--

squeezeToString :: [String] -> String
squeezeToString str = case str of
    [] -> ""
    (x:xs) -> x ++ (squeezeToString xs)

parseWordPhase4 :: String -> String
parseWordPhase4 w = 
    if isWhiteSpace (head w)
        then dropWhile isWhiteSpace w
        else w

parseWordPhase3 :: String -> String
parseWordPhase3 w = 
    if (head w) == '='
        then parseWordPhase4 (tail w)
        else ""

parseWordPhase2 :: String -> String
parseWordPhase2 w =
    if isWhiteSpace (head w)
        then parseWordPhase3 w'
        else parseWordPhase3 w'
        where w' = dropWhile isWhiteSpace w

parseWordPhase1 :: String -> String
parseWordPhase1 w = 
    if (head w) == 'w'
        then parseWordPhase2 (tail w)
        else ""

parseWord :: String -> String
parseWord w = 
    if ('=' `elem` w) && isWhiteSpace (head w)
        then parseWordPhase1 w'
        else if ('=' `elem` w) && (head w) == 'w'
        then parseWordPhase1 w
            else w
            where w' = dropWhile isWhiteSpace w


--

parseAlphabetPhase4 :: String -> String
parseAlphabetPhase4 a = 
    if (head a) == '='
        then (tail a)
        else ""

parseAlphabetPhase3 :: String -> String
parseAlphabetPhase3 a =
    if (head a) == '='
        then (tail a)
        else if isWhiteSpace (head a)
            then parseAlphabetPhase4 a' 
            else ""
            where a' = dropWhile isWhiteSpace a

parseAlphabetPhase2 :: String -> String
parseAlphabetPhase2 a =
    if (head a) == 'A'
        then parseAlphabetPhase3 (tail a)
        else "" 

parseAlphabetPhase1 :: String -> String
parseAlphabetPhase1 a =
    if (head a) == '{'
        then a
        else if isWhiteSpace (head a)
            then parseAlphabetPhase2 a'
            else parseAlphabetPhase2 a
            where a' = dropWhile isWhiteSpace a

parseAlphabet :: String -> [String]
parseAlphabet a = case dropWhile isUnnecessery a of
    "" -> []
    s' -> l : parseAlphabet a'' where (l, a'') = break isUnnecessery s'

readProd prods count letters = do
    newProd <- getLine
    let prodWithNoNums = deleteDigitsFromProd newProd
        (letters', parsedProd) = parseProduction prodWithNoNums letters ""
        newProds = prods ++ [parsedProd]
        prodCount = length newProds 
    if prodCount == count
        then return (letters', newProds)
        else readProd newProds count letters'
-- Parsing input (done manually)
------------------------------------------------
-- DANGER ZONE




-- Main I/O function invoking other functions
readInputPrintOutput = do
    a <- getLine
    w <- getLine
    let alphSet = parseAlphabetPhase1 a
        alph = parseAlphabet alphSet
        alphabet = squeezeToString alph
        word = parseWord w
    (letters, prods) <- readProd [] (length alphabet) ""
    print letters
    print prods
    let productions = if (length letters) == (length alphabet)
        then zip letters prods
        else zip alphabet prods
        inDeptSet = calculateIndependencySet productions alphabet 
        deptSet = calculateDependencySet productions alphabet
        graph = createDotGraph deptSet word
        edges = fst graph
        fnfRes = fnf inDeptSet word "" 
        graphDerivedFnf = fnfDerivedFromMinimizedGraph graph
    putStrLn "D ="
    print deptSet
    putStrLn "I ="
    print inDeptSet 
    putStrLn "FNF ="
    print fnfRes
    putStrLn "Minimized graph in Dot format ="
    printGraphInDotFormat graph
    putStrLn "FNF derived from graph ="
    print graphDerivedFnf
    writeDotGraphToFile graph "graph.gv"
    writeInfoToFile deptSet inDeptSet fnfRes graphDerivedFnf "freshResults.txt"
    

-- Main
start = do
    putStrLn "Format:"
    putStrLn "[A = ]{<set of operations separated with commas>}"
    putStrLn "[w = ]<word that contains only letters from alphabet A>"
    putStrLn "List of operations related to letters from alphabet in formula:"
    putStrLn "[(<letter from alphabet>) ]<variable> := <arithmetical operation>"
    readInputPrintOutput
main = start