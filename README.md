# DependenViz

This executable reads an input file in a simple DSL and,
using the graphviz (https://gitlab.com/daniel-casanueva/haskell/graphviz) bindings,
outputs a visualisation of the specified dependency trees

## Installation

DependenViz requires a Graphviz installation for it to work.

Link to the [Graphviz installation page](https://graphviz.org/download/)

To install DependenViz on your system run `cabal install .` in the project folder

To build it run `cabal build`, then `cabal run dependenViz -- [input] (OPTIONS)`

## Usage

dependenViz FILENAME \[-o|--output File] \[-t|--type Extension]

Available options:
  FILENAME                 Name of the file to process
  -o,--output File         Optional name of output file. Default (FILENAME)
  -t,--type Extension      Optional extension of output file. Default 'png'
  -h,--help                Show this help text

Example input file in the DependenViz language

docs/prueba.dvz

```
(TEST; color: all only LightBlue)

Base {
    -- comments start with '--'
    rank: 1,
    -- optional
    color: Blue,
}

Base2 { }

dependency1 {
    depends: [Base],
}

dependency2 {
    rank: 3,
    depends: [Base2]
}

dependency3 {
    depends: [dependency1, dependency2]
}
```

When called with `cabal run dependenViz -- docs/prueba.dvz -o output`,
or with `dependenViz input.dvz` after installing with `cabal install .`, this outputs

output.png

![output image](docs/prueba.png)

