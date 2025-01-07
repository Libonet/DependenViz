# DependenViz

This executable reads an input file in a simple DSL and,
using the graphviz (https://gitlab.com/daniel-casanueva/haskell/graphviz) bindings,
outputs a visualisation of the specified dependency trees

## Installation

DependenViz requires a Graphviz installation for it to work.

Link to the [Graphviz installation page](https://graphviz.org/download/)

To install DependenViz run `cabal install`

To build it run `cabal build`, then `cabal run`

## Usage

Example input file in the DependenViz language

input.dvz
```
projectName: Test

Base {
    -- comments start with '--'
    rank: 0,
    -- optional
    color: blue,
}

Base2 { }

dependency1 {
    depends: [Base],
}

dependency2 {
    rank: 2,
    depends: [Base2],
}

dependency3 {
    depends: [dependency1, dependency2],
}
```


