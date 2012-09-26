liPL
====

LIST OF FILES
=============

	- src/Main.hs       Main program.
	- src/LiPL.hs       Definition of the language liPL.
	- src/Evaluator.hs  Interpreter for liPL.
	- src/TypeCheck.hs  Type checking and inference for liPL.
	- src/Parser.hs     Parser of liPL programs.

	- doc/index.html  Index of the documentation.

	- examples/map.lipl     Example using map.
	- examples/filter.lipl  Example using filter.
	- examples/length.lipl  Example using filter and length.
	- examples/curry.lipl   Example using map and currification.

HOW TO COMPILE
==============

First of, you need to install the Haskell platform, which can be downloaded from http://hackage.haskell.org/platform/ .

Once it is downloaded, you can must install the following package: parsec

In order to do so, you can simply type the following command once the haskell platform has been installed:

	cabal install parsec

Then, once the installation is done, you can simply compile the program using the following command in the root directory of the project:

	ghc src/*.hs -o lipl -hidir buildfiles -odir buildfiles

This will compile the haskell program into an executable file called lipl.

TRYING OUT LIPL PROGRAMS
========================

Once you have compiled lipl, you can evaluate programs simply doing:

	./lipl examples/map.lipl

For instance.

You can add the -c argument to see the constraints and the -d to display the typed code.

	./lipl examples/map.lipl -c -d

Have fun!