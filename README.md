# WACC Compiler

> This compiler was written as a part of the second year undergraduate computing compilers project by Codrin Cotarlan
> (@cc1619), Daria Petca (@dp1119), Gavin Wu (@gjw19) and Sudarshan Sreeram (@ss8119). Note that this software is
> provided without warranty of any kind; the developers are not held liable for any damages in through use of this code.

The project follows the standard SBT structure; the build configuration file `build.sbt` is present in the root folder.
If using IntelliJ IDEA, open the project from that file. To compile the project, run `make` in the root directory; this
calls sbt to compile and assemble the project. To run the project, invoke the `./compile` command with the required
arguments (`.wacc` files).
