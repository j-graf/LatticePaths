newPackage(
    "LatticePaths",
    Version => "0.1",
    Date => "June 30, 2025",
    Authors => {
	{Name => "John Graf", Email => "grafjohnr@gmail.com", HomePage => "https://j-graf.github.io/"}},
    Headline => "a package for constructing lattice paths",
    Keywords => {"Combinatorics"},
    DebuggingMode => false
    )

export {"LatticePath", "latticePath", "LatticeNPath", "latticeNPath",
        "isNE", "isNW", "isSE", "isSW", "isCardinal", "isIntersecting",
        "stepList", "weight", "type", "xMin", "xMax", "yMin", "yMax",
        "cardinalToSteps", "cardinalToChars",
        "allPaths", "JTlatticeNPaths", "toSSYT"}

needsPackage "Permutations"
needsPackage "SkewTableaux"

WeightRing = ZZ[x_1..x_20]

LatticePath = new Type of BasicList

-- constructors

latticePath = method(TypicalValue => LatticePath)
latticePath List := vectorList -> (
    if any(vectorList,theVec -> not instance(theVec,Sequence)) then error "expected list of sequences";
    if any(vectorList,theVec -> #theVec != #vectorList#0) then error "expected all vectors to have the same dimension";
    --if (any(seq,theVec -> #theVec != 2)) then error "expected all vectors to have dimension 2";
    lattice := new LatticePath from for theElt in vectorList list toSequence theElt;
    
    lattice
    )
-*
latticePath (Sequence,List) := (startPoint,theWord) -> (
    if #startPoint != 2 then error "expected 2D starting point";
    ans := {toList startPoint};

    letterToStep := new HashTable from {
        "N" => {0,1},
        "n" => {0,1},
        "E" => {1,0},
        "e" => {1,0},
        "S" => {0,-1},
        "s" => {0,-1},
        "W" => {-1,0},
        "w" => {-1,0}};
    
    for theLetter in theWord do (
        ans = append(ans,ans#-1 + letterToStep#theLetter);
        );

    latticePath for theVec in ans list toSequence theVec
    )
*-
latticePath (Sequence,String) := (startPoint,theWord) -> (
    if #startPoint != 2 then error "expected 2D starting point";
    ans := {toList startPoint};

    letterToStep := new HashTable from {
        "N" => {0,1},
        "n" => {0,1},
        "E" => {1,0},
        "e" => {1,0},
        "S" => {0,-1},
        "s" => {0,-1},
        "W" => {-1,0},
        "w" => {-1,0}};
    
    for theLetter in theWord do (
        ans = append(ans,ans#-1 + letterToStep#theLetter);
        );

    latticePath for theVec in ans list toSequence theVec
    )

-- methods

-- input: a LatticePath
-- output: a List of steps, i.e., the difference between consecutive vectors in the lattice
--     steps are sequences (..)
stepList = method(TypicalValue => List)
stepList LatticePath := lattice -> (
    for i from 0 to #lattice-2 list toSequence(toList(lattice#(i+1))-toList(lattice#i))
    )

-- input: a LatticePath
-- output: true if the lattice has only NE steps, false otherwise
isNE = method(TypicalValue => Boolean)
isNE LatticePath := lattice -> (
    theStepList := stepList lattice;
    all(theStepList,theStep -> isMember(theStep,{(1,0),(0,1)}))
    )

isNW = method(TypicalValue => Boolean)
isNW LatticePath := lattice -> (
    theStepList := stepList lattice;
    all(theStepList,theStep -> isMember(theStep,{(-1,0),(0,1)}))
    )

isSE = method(TypicalValue => Boolean)
isSE LatticePath := lattice -> (
    theStepList := stepList lattice;
    all(theStepList,theStep -> isMember(theStep,{(1,0),(0,-1)}))
    )

isSW = method(TypicalValue => Boolean)
isSW LatticePath := lattice -> (
    theStepList := stepList lattice;
    all(theStepList,theStep -> isMember(theStep,{(-1,0),(0,-1)}))
    )

isCardinal = method(TypicalValue => Boolean)
isCardinal LatticePath := lattice -> (
    theStepList := stepList lattice;
    all(theStepList,theStep -> isMember(theStep,{(1,0),(-1,0),(0,1),(0,-1)}))
    )

-- binary operators

-- the dimension of a LatticePath, i.e., the dimension of its vectors
dim LatticePath := lattice -> (
    if #lattice == 0 then return 0;
    #lattice#0
    )

net LatticePath := lattice -> (
    if #lattice == 0 then return "∅";
    ans := toString(lattice#0);
    for i from 1 to #lattice-1 do (
        ans = ans|" -> "|toString(lattice#i);
        );
    ans
    )

toString LatticePath := lattice -> (
    "LatticePath: "|(net lattice)
    )

LatticePath == LatticePath := (lattice1,lattice2) -> (
    if #lattice1 != #lattice2 or dim lattice1 != dim lattice2 then return false;
    for i from 0 to #lattice1-1 do (
        if lattice1#i != lattice2#i then return false;
        );
    true
    )

---------- LatticeNPath

LatticeNPath = new Type of BasicList

-- constructors

latticeNPath = method(TypicalValue => LatticeNPath)

latticeNPath List := seq -> (
    seq = sequence seq;
    if instance(seq#0,List) and #seq==1 then (
        seq = toSequence seq#0;
        );
    if not all(seq,theLattice -> instance(theLattice, LatticePath)) then error "expected list with elements of type LatticePath";
    if (any(seq,theLattice -> dim theLattice != dim seq#0)) then error "expected all vectors to have the same dimension";
    nPath := new LatticeNPath from seq;
    
    nPath
    )

-- methods

-- the dimension of a LatticeNPath, i.e., the dimension of its vectors
dim LatticeNPath := nPath -> dim nPath#0

net LatticeNPath := nPath -> (
    if #nPath == 0 then return "[]";
    ans := net nPath#0;
    for i from 1 to #nPath-1 do (
        ans = ans||(net nPath#i)
        );
    
    lBrak := "[ ";
    rBrak := " ]";
    if #nPath >= 2 then (
        lBrak = "┌ ";
        rBrak = " ┐";
        for i from 1 to #nPath-2 do (
            lBrak = lBrak||"│ ";
            rBrak = rBrak||" │";
            );
        lBrak = lBrak||"└ ";
        rBrak = rBrak||" ┘";
        );

    lBrak|ans|rBrak
    )

toString LatticeNPath := nPath -> "-*a LatticeNPath with "|toString(#nPath)|" paths*-"

-- binary operators

---------- methods for both

-- input: LatticeNList or Sequence/List of Type LatticeList
-- output: true if lattices are intersecting, false if lattices are non-intersecting
isIntersecting = method(TypicalValue => Boolean)
isIntersecting LatticeNPath := nPath -> (
    -*
    latticeList = sequence latticeList;
    if (instance(latticeList#0,List) or instance(latticeList#0,LatticeNPath)) and #latticeList==1 then (
        latticeList = toSequence latticeList#0;
        );
    *-
    allVecs := flatten for theLattice in nPath list unique for theVec in theLattice list theVec;
    
    (#allVecs > #(unique allVecs))
    )
isIntersecting List := latticeList -> (
    if not all(latticeList, theLattice -> instance(theLattice,LatticePath)) then error "expected list of lattices";
    isIntersecting latticeNPath latticeList
    )
isIntersecting (LatticePath,LatticePath) := (lattice1,lattice2) -> (
    isIntersecting latticeNPath {lattice1,lattice2}
    )

-- smallest x-value in a LatticePath or LatticeNPath
xMin = method(TypicalValue => ZZ)
xMin LatticePath := theInput -> (
    min for theVec in theInput list theVec#0
    )
xMin LatticeNPath := theInput -> (
    min for theLattice in theInput list xMin theLattice
    )

-- largest x-value in a LatticePath or LatticeNPath
xMax = method(TypicalValue => ZZ)
xMax LatticePath := lattice -> (
    max for theVec in lattice list theVec#0
    )
xMax LatticeNPath := theInput -> (
    max for theLattice in theInput list xMax theLattice
    )

-- smallest y-value in a LatticePath or LatticeNPath
yMin = method(TypicalValue => ZZ)
yMin LatticePath := lattice -> (
    min for theVec in lattice list theVec#1
    )
yMin LatticeNPath := theInput -> (
    min for theLattice in theInput list yMin theLattice
    )

-- largest y-value in a LatticePath or LatticeNPath
yMax = method(TypicalValue => ZZ)
yMax LatticePath := lattice -> (
    max for theVec in lattice list theVec#1
    )
yMax LatticeNPath := theInput -> (
    max for theLattice in theInput list yMax theLattice
    )

-- type of LatticePath or LatticeNPath [EC1, p.246]
type = method(TypicalValue => Sequence)
type LatticeNPath := nPath -> (
    alpha := toSequence for theLattice in nPath list theLattice#-1#0;
    beta := toSequence for theLattice in nPath list theLattice#0#0;
    gamma := toSequence for theLattice in nPath list theLattice#0#1;
    delta := toSequence for theLattice in nPath list theLattice#-1#1;

    (alpha,beta,gamma,delta)
    )
type LatticePath := lattice -> type latticeNPath {lattice}

tex LatticeNPath := latticeList -> (
    if dim latticeList != 2 then error "expected lattice to have dimension 2";
    
    isGrid := false;
    isNumberedHoriz := true;
    isNumberedVert := true;
    
    dotSize := "2pt";  -- or "ultra thick"
    pathThickness := "line width=4pt";
    colorList := {"red","blue","green","orange","magenta"};

    ans := "\\begin{tikzpicture}\n";

    xMinGrid := xMin latticeList;--min for theLattice in latticeList list xMin theLattice;
    xMaxGrid := xMax latticeList;--max for theLattice in latticeList list xMax theLattice;
    yMinGrid := yMin latticeList;--min for theLattice in latticeList list yMin theLattice;
    yMaxGrid := yMax latticeList;--max for theLattice in latticeList list yMax theLattice;

    -- draw grid
    if isGrid then (
	ans = ans|"    \\draw[gray] ("|toString(xMinGrid)|","|toString(yMinGrid)|") grid ("|toString(xMaxGrid)|","|toString(yMaxGrid)|");\n";
	);

    -- draw lattice points
    ans = ans|"    \\foreach \\i in {"|toString(xMinGrid)|",...,"|toString(xMaxGrid)|"}\n        \\foreach \\j in {"|toString(yMinGrid)|",...,"|toString(yMaxGrid)|"}\n            \\fill (\\i,\\j) circle ("|dotSize|");\n";

    -- draw labels
    if isNumberedHoriz then (
	ans = ans|"    \\foreach \\i in {"|toString(xMinGrid)|",...,"|toString(xMaxGrid)|"}\n        \\fill (\\i,"|toString(yMinGrid)|") node[below] {\\i};\n";
	);
    if isNumberedVert then (
	ans = ans|"    \\foreach \\j in {"|toString(yMinGrid)|",...,"|toString(yMaxGrid)|"}\n        \\fill ("|toString(xMinGrid)|",\\j) node[left] {\\j};\n";
	);

    for iInv from 1 to #latticeList do (
        i := #latticeList - iInv;

        theLattice := latticeList#i;
	
        theColor := colorList#(i%(#colorList));
        ans = ans|"    \\draw["|pathThickness|","|theColor|"]\n";
        
        currPath := "        ";
        for j from 0 to #theLattice-1 do (
            theVec := theLattice#j;
            currPath = currPath|toString(theVec);
            if j < #theLattice-1 then (
                currPath = currPath|" -- ";
                );
            );
        currPath = currPath|";\n";

            
        currPath = currPath|"    \\fill["|theColor|"]\n";
        currPath = currPath|"        ("|toString(theLattice#0#0)|","|toString(theLattice#0#1)|") circle (3pt)\n";
        currPath = currPath|"        ("|toString(theLattice#-1#0)|","|toString(theLattice#-1#1)|") circle (3pt);\n";
    
        ans = ans|currPath;
        );

    ans = ans|"\\end{tikzpicture}";
    ans
    )

tex LatticePath := lattice -> (
    if dim lattice != 2 then error "expected lattice to have dimension 2";

    tex latticeNPath {lattice}
    )

-- weight of a LatticePath or LatticeNPath [EC1, p.246]
weight = method(TypicalValue => Sequence)
weight LatticePath := lattice -> (
    theSteps := stepList lattice;
    if not all(theSteps, aStep -> isMember(aStep,{(1,0),(-1,0),(0,1),(0,-1)})) then error "expected only steps NESW";

    horizHeights := for i from 0 to #theSteps-1 list (
        if theSteps#i == (1,0) or theSteps#i == (-1,0) then (
            lattice#i#1
            ) else (
            continue
            )
        );
    
    if #horizHeights == 0 then return toSequence horizHeights;
    toSequence for i from 1 to max horizHeights list number(horizHeights, theHeight -> theHeight == i)
    )
weight LatticeNPath := nPath -> (
    weightList := for thePath in nPath list weight thePath;
    maxLen := max for theWeight in weightList list #theWeight;

    toSequence sum for theWeight in weightList list (toList(theWeight)|toList((maxLen-#theWeight):0))
    )

-- input: two lattice paths
-- output: sequence of two lattice paths
--     let v be the first vector of lattice1 that is shared by both lattices,
--         say a1 -> v -> a3 -> ... and b1 -> v -> b3 -> ...
--     output is the list of paths a1 -> v -> b3 -> ... and b1 -> v -> a3 -> ...
--     if no such v exists, then returns (lattice1,lattice2)
swapTails = method(TypicalValue => List)
swapTails (LatticePath,LatticePath) := (lattice1,lattice2) -> (
    theIndex1 := position(toList lattice1,theVec -> isMember(theVec,toList lattice2));
    theIndex2 := position(toList lattice2,theVec -> theVec == lattice1#theIndex1);
    if theIndex1 === null then return (lattice1,lattice2);

    new1 := latticePath ((for i from 0 to theIndex1 list lattice1#i)|(for i from theIndex2+1 to #lattice2-1 list lattice2#i));
    new2 := latticePath ((for i from 0 to theIndex2 list lattice2#i)|(for i from theIndex1+1 to #lattice1-1 list lattice1#i));

    (new1,new2)
    )

-- algorithms

-- input: the type of a LatticeNPath, i.e., four n-tuples of integers [EC1, p.246]
--     theCardinal: string of "N","S","E","W" of length 2
-- output: List of all 'words sequences' that describe LatticeNPaths of given type
--     'words' are a List of characters "S" and "E", e.g., {"S","S","E","S","E"}
--     'word sequences' are sequences of length n of 'words'
allWords = method(TypicalValue => List)
allWords (Sequence,String) := (theType,theCardinal) -> (
    if all(theType,theEntry -> instance(theEntry,ZZ)) then (
        theType = for theEntry in theType list sequence theEntry;
        );
    if #theType!=4 or any(theType,theEntry -> #theEntry != #theType#0) then (
        error "not a valid type for a LatticeNPath"
        );

    (xStep,yStep) := cardinalToSteps theCardinal;
    yLetter := theCardinal#0;
    xLetter := theCardinal#1;
    
    wordList := for i from 0 to #theType#0-1 list (
        yChange := theType#3#i - theType#2#i; -- delta-gamma
        xChange := theType#0#i - theType#1#i; -- alpha-beta
        
        yNumSteps := yChange//yStep;
        xNumSteps := xChange//xStep;

        if yNumSteps < 0 or xNumSteps < 0 then (
            {}
            ) else (
            uniquePermutations(toList(yNumSteps:yLetter)|toList(xNumSteps:xLetter))
            )
        );

    if #wordList == 1 then return(apply(wordList#0, theWord -> sequence theWord));
    
    ans := wordList#0;
    for i from 1 to #wordList-1 do (
        ans = ans ** wordList#i;
        );

    ans/deepSplice
    )

-- output: (xStep,yStep), where xStep = +-1 and yStep = +-1
cardinalToSteps = method(TypicalValue => Sequence)
cardinalToSteps String := theCard -> (
    if #theCard !=2 then error "expected string of length 2";
    numN := number({0,1}, i -> isMember(theCard#i,{"N","n"}));
    numS := number({0,1}, i -> isMember(theCard#i,{"S","s"}));
    numE := number({0,1}, i -> isMember(theCard#i,{"E","e"}));
    numW := number({0,1}, i -> isMember(theCard#i,{"W","w"}));

    if numN + numS != 1 then error "expected exactly one character N or S";
    if numE + numW != 1 then error "expected exactly one character E or W";

    ((-1)^numW,(-1)^numS)
    )

cardinalToChars = method(TypicalValue => Sequence)
cardinalToChars String := theCard -> (
    if #theCard !=2 then error "expected string of length 2";
    numN := number({0,1}, i -> isMember(theCard#i,{"N","n"}));
    numS := number({0,1}, i -> isMember(theCard#i,{"S","s"}));
    numE := number({0,1}, i -> isMember(theCard#i,{"E","e"}));
    numW := number({0,1}, i -> isMember(theCard#i,{"W","w"}));

    if numN + numS != 1 then error "expected exactly one character N or S";
    if numE + numW != 1 then error "expected exactly one character E or W";

    yChar := if numN == 1 then "N" else "S";
    xChar := if numE == 1 then "E" else "W";

    (yChar,xChar)
    )


-- input: Sequence startPoint in ZZ^2, and List theWord containing characters "N", "S", "E", or "W"
--     e.g., {"N","N","E","N","E"}
-- output: LatticePath starting at startPoint, with steps following theWord
wordToPath = method(TypicalValue => LatticePath)
wordToPath (Sequence,List) := (startPoint,theWord) -> (
    if #startPoint != 2 then error "expected 2D starting point";
    ans := {toList startPoint};

    letterToStep := new HashTable from {
        "N" => {0,1},
        "n" => {0,1},
        "E" => {1,0},
        "e" => {1,0},
        "S" => {0,-1},
        "s" => {0,-1},
        "W" => {-1,0},
        "w" => {-1,0}};
    
    for theLetter in theWord do (
        ans = append(ans,ans#-1 + letterToStep#theLetter);
        );

    latticePath for theVec in ans list toSequence theVec
    )

-- input: the type of a LatticeNPath, i.e., four n-tuples of integers [EC1, p.246]
--     yLetter is "N" or "S"
--     xLetter is "E" or "W"
-- output: list of all LatticeNPaths of the given type
--     paths are SE
allPaths = method(TypicalValue => List)
allPaths (Sequence,String) := (theType,theCardinal) -> (
    (yLetter,xLetter) := cardinalToChars theCardinal;
    
    wordListList := allWords(theType,theCardinal);
    beta := theType#1;
    gamma := theType#2;
    if instance(beta,ZZ) then (
        beta = {beta};
        gamma = {gamma};
        );
    ans := for wordList in wordListList list (
        latticeNPath for i from 0 to #wordList-1 list (
            wordToPath((beta#i,gamma#i), wordList#i)
            )
        );

    ans
    )

-- input: integer n
-- output: list of all permutations of S_n
permList = method(TypicalValue => List)
permList ZZ := n -> for thePerm in permutations n list permutation (thePerm + toList(n:1))

-- input: Permutation p and List {a_1,a_2,...}
-- output: a List {a_p(1),a_p(2),...}
actOnIndex = method(TypicalValue => List)
actOnIndex (Permutation,List) := (p,theList) -> (
    if #p != #theList then error "expected permuation and list to have same length";
    for iMap in p list theList#(iMap-1)
    )

-- input: Partitions lam,mu,theVersion = "EC2" or "website" integer N (optional)
-- output: Bag of LatticeNPaths arising from Lindström–Gessel–Viennot lemma for S_{lam/mu}
--     using the bijection described in [EC1, p.246-248] and [EC2, p.377-378]
--     paths are SE
JTlatticeNPaths = method(TypicalValue => List)
JTlatticeNPaths (Partition,Partition,ZZ) := (lam,mu,N) -> (
    --if N < #lam then return(Bag {});
    mu = toList(mu)|toList((#lam-#mu):0);
    n := #lam;

    alpha := for j from 0 to #lam-1 list (
        (lam#j+n-(j+1))
        );
    beta := for i from 0 to #mu-1 list (
        (mu#i+n-(i+1))
        );
    gamma := toList((#alpha):N);
    delta := toList((#beta):1);
    theType := (alpha,beta,gamma,delta);
    print theType;
    

    thePermList := permList n;

    typeList := for thePerm in thePermList list (actOnIndex(thePerm,alpha),beta,gamma,actOnIndex(thePerm,delta));

    Bag flatten apply(typeList, theType -> allPaths(theType,"SE"))
    )
JTlatticeNPaths (Partition,Partition) := (lam,mu) -> (
    JTlatticeNPaths(lam,mu,#lam)
    )

toSSYT = method(TypicalValue => Sequence)
toSSYT LatticeNPath := nPath -> (
    (alpha,beta,gamma,delta) := type nPath;
    
    if not all(gamma,gam -> gam == gamma#0) then error "expected all paths to start at (x,N)";
    if not all(delta,del -> del == delta#0) then error "expected all paths to end at (x,1)";
    
    n := #nPath; --number of SSYT rows
    N := gamma#0; --number of variables

    lam := for j from 0 to #alpha-1 list (alpha#j-n+j+1);
    mu := for i from 0 to #beta-1 list (beta#i-n+i+1);
    
    
    ans := for i from 0 to n-1 list (
        lamPart := new Partition from {lam#i};
        muPart := new Partition from {mu#i};
        theWeight := toList weight nPath#i;
        entryList := flatten for i from 0 to #theWeight-1 list toList((theWeight#i):(i+1));
    
        skewTableau(lamPart, muPart, entryList)
        );

    return toSequence ans
    )


end--
