# Lattice Paths

Package (work-in-progress) for dealing with lattice paths.

# Examples

## Lattice path basics

- Create a Lattice Path with vectors (vertices) $(1,2)\to(5,3)\to(6,-1)\to(0,0)\to(-1,-1)$:
```
thePath = latticePath {(1,2),(5,3),(6,-1),(0,0),(-1,-1)}
```

- Compute the list of steps, i.e., the differences between successive vectors:
```
stepList thePath
```

- Check if a path only has North and East steps:
```
isNorthEast thePath
isNorthEast latticePath {(1,4),(2,4),(2,5),(2,6)}
```

- Get the dimension of a path, i.e., the dimension of its vectors:
```
dim thePath
```

## Lattice $n$-path basics

- Create a $3$-path
```
thePath1 = latticePath {(1,2),(5,3),(6,-1),(0,0),(-1,-1)}
thePath2 = latticePath {(1,4),(2,4),(2,5),(2,6)}
thePath3 = latticePath {(0,0),(-5,3),(6,6),(7,5),(1,1),(2,2)}
theNPath = latticeNPath {thePath1,thePath2,thePath3}
```

- Get the dimension of an $n$-path, i.e., the dimension of its paths:
```
dim theNPath
```

## Methods

- Check if there are any intersections in an $n$-path, or in a list/sequence of paths:
```
isIntersecting theNPath
isIntersecting {thePath1,thePath2}
isIntersecting (thePath1,thePath3)
```

- Get the max/min x and y values of a path or $n$-path:
```
xMin thePath1
xMax theNPath
yMin thePath2
yMax thePath
```

- Get the type of an $n$-path or path (the starting and endpoints of an $n$-path, \[see EC1, p.246\]):
```
type thePath
type theNPath
```

- Get LaTeX TikZ code for the path or $n$-path:
```
tex thePath
tex theNPath
```

- Get the weight of an $n$-path or path \[see EC1, p.246\], assuming the steps are all NESW:
```
weight thePath
weight theNPath
```

- If two paths intersect, swap their 'tails':
```
thePath4 = latticePath{(1,2),(3,4),(5,6)}
thePath5 = latticePath{(0,1),(1,1),(3,4),(6,7)}
swapTails(thePath4,thePath5)
```

## Algorithms

- List all paths (or $n$-paths) with the given type, and the given steps
```
pathType = (3,0,5,3)
allPaths(pathType,"S","E")
nPathType = ((3,2),(0,0),(5,2),(3,0))
allPaths(nPathType,"S","E")
```

- Bagged list all $n$-paths arising from the Lindström–Gessel–Viennot lemma for computing the skew Schur function $S_{\lambda/\mu}(x_1,\ldots,x_n)$ (by default $n=\ell(\lambda)$ if $n$ is not provided) (see \[EC1, p.246-248\] and \[EC2, p.377-378\]):
```
lam = {3,2}
mu = {1}
n = 3
JTlatticeNPaths(lam,mu,n)
theNPaths = JTlatticeNPaths(lam,mu)
```

- Output all $n$-paths (and their weights) from LGV lemma to a .tex file:
```
f = "output"|toString(currentTime())|".tex"
for nPath in theNPaths do (
    f << tex theLattice << endl;
    f << tex weight theLattice << endl << endl;
    )
f << close
```
