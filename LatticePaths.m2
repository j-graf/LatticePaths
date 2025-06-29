
needsPackage "Permutations"

WeightRing = ZZ[x_1..x_20]



LatticePath = new Type of BasicList

-- constructors

latticePath = method(TypicalValue => LatticePath)

latticePath List := seq -> (
    if (any(seq,theVec -> #theVec != #seq#0)) then error "expected all vectors to have the same dimension";
    --if (any(seq,theVec -> #theVec != 2)) then error "expected all vectors to have dimension 2";
    lattice := new LatticePath from for theElt in seq list toSequence theElt;
    
    lattice
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
isNorthEast = method(TypicalValue => Boolean)
isNorthEast LatticePath := lattice -> (
    theStepList := stepList lattice;
    not any(theStepList,theStep -> not isMember(theStep,{(1,0),(0,1)}))
    )

-- binary operators

-- the dimension of a LatticePath, i.e., the dimension of its vectors
dim LatticePath := lattice -> #lattice#0

---------- LatticeNPath

LatticeNPath = new Type of BasicList

-- constructors

latticeNPath = method(TypicalValue => LatticeNPath)

latticeNPath := seq -> (
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

-- binary operators

---------- methods for both

-- input: LatticeNList or Sequence/List of Type LatticeList
-- output: true if lattices are intersecting, false if lattices are non-intersecting
isIntersecting = method(TypicalValue => Boolean)
isIntersecting := latticeList -> (
    latticeList = sequence latticeList;
    if (instance(latticeList#0,List) or instance(latticeList#0,LatticeNPath)) and #latticeList==1 then (
        latticeList = toSequence latticeList#0;
        );
    allVecs := flatten for theLattice in latticeList list unique for theVec in theLattice list theVec;
    
    (#allVecs > #(unique allVecs))
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
type LatticePath := lattice -> type latticeNPath lattice

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

    tex latticeNPath lattice
    )

-- weight of a LatticePath or LatticeNPath [EC1, p.246]
weight = method(TypicalValue => ring x_1)
weight LatticePath := lattice -> (
    ans := 1;
    
    theSteps := stepList lattice;
    ans = product for i from 0 to #theSteps-1 list (
        if theSteps#i == (1,0) then (
            x_(lattice#i#1)
            ) else (
            1
            )
        );

    ans
    --if ans == 1 then return(0) else return(ans);
    )
weight LatticeNPath := nPath -> (
    product for lattice in nPath list weight lattice
    )


-- algorithms

-- input: the type of a LatticeNPath, i.e., four n-tuples of integers [EC1, p.246]
--     yLetter, either "N" or "S", the direction paths move in the y-direction
--     xLetter, either "E" or "W", the direction paths move in the x-direction
-- output: List of all 'words sequences' that describe LatticeNPaths of given type
--     'words' are a List of characters "S" and "E", e.g., {"S","S","E","S","E"}
--     'word sequences' are sequences of length n of 'words'
allWords = method(TypicalValue => List)
allWords (Sequence,String,String) := (theType,yLetter,xLetter) -> (
    if all(theType,theEntry -> instance(theEntry,ZZ)) then (
        theType = for theEntry in theType list sequence theEntry;
        );
    if #theType!=4 or any(theType,theEntry -> #theEntry != #theType#0) then (
        error "not a valid type for a LatticeNPath"
        );
    
    yLetterToStep := new HashTable from {
        "N" => {0,1},
        "n" => {0,1},
        "S" => {0,-1},
        "s" => {0,-1}};
    xLetterToStep := new HashTable from {
        "E" => {1,0},
        "e" => {1,0},
        "W" => {-1,0},
        "w" => {-1,0}};
    
    yStep := yLetterToStep#yLetter;
    xStep := xLetterToStep#xLetter;
    
    wordList := for i from 0 to #theType#0-1 list (
        yChange := theType#3#i - theType#2#i; -- delta-gamma
        xChange := theType#0#i - theType#1#i; -- alpha-beta
        
        yNumSteps := yChange//yStep#1;
        xNumSteps := xChange//xStep#0;

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

-*
-- input: the type of a LatticeNPath, i.e., four n-tuples of integers [EC1, p.246]
-- output: List of all 'words' that describe LatticeNPaths of given type, with steps SE
--     'words' are a List of characters "S" and "E", e.g., {"S","S","E","S","E"}
allWordsSE = method(TypicalValue => List)
allWordsSE := (alpha,beta,gamma,delta) -> (
    if (instance(alpha,ZZ) and instance(beta,ZZ) and instance(gamma,ZZ) and instance(delta,ZZ)) then (
        alpha = sequence alpha;
        beta = sequence beta;
        gamma = sequence gamma;
        delta = sequence delta;
        );
        
    if not (#alpha == #beta and #alpha == #gamma and #alpha == #delta) then (
        error "not a valid path type";
        );
    
    wordList := for i from 0 to #alpha-1 list (
        numS := gamma#i - delta#i;
        numE := alpha#i - beta#i;

        uniquePermutations(toList(numS:"S")|toList(numE:"E"))
        );

    if #wordList == 1 then return(apply(wordList#0, theWord -> sequence theWord));
    
    ans := wordList#0;
    for i from 1 to #wordList-1 do (
        ans = ans ** wordList#i;
        );

    ans/deepSplice
    )
*-

-- input: Sequence startPoint in ZZ^2, and List or String theWord containing characters "N", "S", "E", or "W"
--     e.g., {"N","N","E","N","E"} or "NNENE"
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

    latticePath ans
    )

-- input: the type of a LatticeNPath, i.e., four n-tuples of integers [EC1, p.246]
-- output: list of all LatticeNPaths of the given type
--     paths are SE
allPaths = method(TypicalValue => List)
allPaths (Sequence,String,String) := (theType,yLetter,xLetter) -> (
    wordListList := allWords(theType,yLetter,xLetter);
    beta := theType#1;
    gamma := theType#2;
   if instance(beta,ZZ) then (
        beta = {beta};
        gamma = {gamma};
        );
    ans = for wordList in wordListList list (
        latticeNPath for i from 0 to #wordList-1 list (
            wordToPath((beta#i,gamma#i),wordList#i)
            )
        );

    ans
    )

-*
-- input: the type of a LatticeNPath, i.e., four n-tuples of integers [EC1, p.246]
-- output: list of all LatticeNPaths of the given type
--     paths are SE
allPathsSE = method(TypicalValue => List)
allPathsSE := (alpha,beta,gamma,delta) -> (
    wordListList := allWordsSE(alpha,beta,gamma,delta);
    if instance(beta,ZZ) then (
        beta = {beta};
        gamma = {gamma};
        );
    ans = for wordList in wordListList list (
        latticeNPath for i from 0 to #wordList-1 list (
            wordToPath((beta#i,gamma#i),wordList#i)
            )
        );

    ans
    )
*-

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

-- input: Partitions lam,mu
-- output: Bag of LatticeNPaths arising from Lindström–Gessel–Viennot lemma for S_{lam/mu}
--     using the bijection described in [EC1, p.246-248] and [EC2, p.377-378]
--     paths are SE
JTlatticeNPaths = method(TypicalValue => List)
JTlatticeNPaths := (lam,mu) -> (
    mu = mu|toList((#lam-#mu):0);
    n := #lam;
    N := #lam;

    alpha := for j from 0 to #lam-1 list (
        (lam#j+n-(j+1))
        );
    beta := for i from 0 to #mu-1 list (
        (mu#i+n-(i+1))
        );
    gamma := toList((#alpha):N);
    delta := toList((#beta):1);
    theType := (alpha,beta,gamma,delta);

    thePermList := permList n;

    typeList := for thePerm in thePermList list (actOnIndex(thePerm,alpha),beta,gamma,actOnIndex(thePerm,delta));

    Bag flatten apply(typeList, theType -> allPaths(theType,"S","E"))
    )



-- examples

lam = {3,2}
mu = {1}

dTest = JTlatticeNPaths(lam,mu)

#dTest
for nPath in dTest do (
    print tex nPath;
    print tex weight nPath;
    print "";
    )






# allWords((3,0,5,3),"S","E")
# allPaths((3,0,5,3),"S","E")





allWordsSE(3,0,5,3)

for theElt in allWords(((3,4),(0,3),(5,2),(3,1)),"S","E") do print theElt
for theElt in allWordsSE((3,4),(0,3),(5,2),(3,1)) do print theElt


allWords((3,0,5,3),"N","E")
allWords((1,0,0,1),"N","E")
for theElt in allWords(((1,0),(0,0),(0,0),(0,0)),"N","E") do print theElt

toSequence {}



















cTest = allPathsSE(3,0,5,3)
for lattice in cTest do (
    print tex lattice;
    print tex weight lattice;
    print "";
    )

aTest = allPathsSE((3,4),(0,3),(5,2),(3,1))
for nPath in aTest do (
    print tex nPath;
    print tex weight nPath;
    print "";
    )


print tex wordToPath((0,5),{"S","S","E","S","E"})
type wordToPath((0,5),{"S","S","E","S","E"})
stepList wordToPath((0,5),{"S","S","E","S","E"})
weight wordToPath((0,5),{"S","S","E","S","E"})


#bTest


-- method examples


youngTableau Partition := p -> (
    tableau:= new YoungTableau;
    tableau#partition = p;
    tableau#values = new MutableList from ((sum toList p):0) ;
    tableau
)



youngTableau(Partition,List):= (p,L)->(
    if(sum toList p != #L) then error " Partition size does not match with the length of the list L";
    tableau:= new YoungTableau;
    tableau#partition =p;
    tableau#values = new MutableList from L;
    tableau
)

entries YoungTableau := tableau -> toList tableau#values


-- examples


testLattice = latticePath {(1,2),(2,3),(9,1)}
toString(testLattice#0)
dim testLattice
print tex testLattice

toSequence(toList(1,2)+toList(5,1))

aList = {latticePath {(1,1),(2,1),(2,2)}, latticePath {(0,0),(3,1),(3,3)}, latticePath {(-1,1),(3,1),(2,2)}}
print latticeListToTex aList







test1 = latticePath {(1,1),(1,2),(2,2)}
test2 = latticePath {(4,1),(2,3),(3,2),(1,1)}
test3 = latticeNPath {test1,test2}



type(latticeNPath{test1})
test4 = type test1

print tex test1


print tex test3


isIntersecting test3
xMin test3
xMax test3
yMin test3
yMax test3
dim test3
type test3



f = method(TypicalValue => ZZ)
f LatticePath := theLattice -> (print theLattice)
f LatticeNPath := nPath -> (print nPath)


latticeNPath test1
test4#0


toSequence {}


theTest = 1
sequence theTest

class (1,2,3,4)


sequence (1,2,3)



aList = Bag unique permutations {"S","S","S","E","E","E"}
for elt in aList do print elt


aList = Bag uniquePermutations((3:"S")|(3:"E"))
for elt in aList do print elt




for theLetter in "Hello" do print(theLetter)




hashTest = new HashTable from {
        "N" => {1,0},
        "E" => {0,1},
        "S" => {-1,0},
        "W" => {0,-1}}

hashTest#"n"


# (({1,2} ** {3,4} ** {5,6} ** {7,8})/deepSplice)


^** {{1,2},{3,4}}


sequence {"N","S","E"}


test5 = permList 3
class test5#0
test5 = permutation {4,1,2,3}
actOnIndex(test5,{2,3,2,5})
