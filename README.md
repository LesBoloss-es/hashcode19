hashcode19
==========

Preparing for the competition
-----------------------------

- Read this README.
- Make sure to have OPAM installed.
- Switch to OCaml 4.07.1.
- Clone the project.
- Install the dependencies: `opam install . --deps-only`.
- Make sure that the project compiles.

Structure of the repository
---------------------------

- The directory `problems` should contain the given problem files.
- The directory `solutions` will be created automatically. It is ignored by the
  versioning system. The tool will write its suggestions there.
- The directory `src` contains the code of the tool including some machinery
  and, later, the definitions of problems, solutions and solvers.

Writing the basics
------------------

The very first thing to do is to write OCaml types for problem and solutions.
These can be found in `Problem.t` and `Solution.t`. One can then parallelise
between writing solvers (cf. next section) and writing a parser for problems
(`Problem.from_file`), a printer for solutions (`Solution.to_file`) and a
scoring function for solutions (`Solution.score`).

Writing a solver
----------------

A solver is a function of type `Problem.t -> Solution.t`. These functions can
(and should) be written in their own modules. They must then be declared in the
sequence `Solvers.all`.
