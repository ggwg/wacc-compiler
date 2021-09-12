# WACC Compiler

> This compiler was written as a part of the second year undergraduate computing compilers project by Codrin Cotarlan
> (@cc1619), Daria Petca (@dp1119), Gavin Wu (@gjw19) and Sudarshan Sreeram (@ss8119). Note that this software is
> provided without warranty of any kind; the developers are not held liable for any damages in through use of this code.

## Building / Setup
The project follows the standard SBT structure; the build configuration file `build.sbt` is present in the root folder.
If using IntelliJ IDEA, open the project from that file. To compile the project, run `make` in the root directory; this
calls sbt to compile and assemble the project. To run the project, invoke the `./compile` command with the required
arguments (`.wacc` files).

## Example Program:
```
begin
  import io_lib # Import pretty array print functions from external library
  
  # Returns a capitalised version of c if b is true and c is a letter
  char conditionalToUpper(char c, bool b) is
    if (b == true && c >= 'a' && c <= 'z') then
      return chr (ord c - 32)
    else
      return c
    fi
  end
  
  # Applies f pairwise between chars and bools, and stores the result in res
  void zipWith(func(char,char,bool) f, char[] res, char[] chars, bool[] bools) is
    for (int i = 0;; i = i + 1) do
      try
        res[i] = call f(chars[i], bools[i]); # Apply f
        continueloop # Continue to the beginning of the loop
      catch
        println "i exceeded array bounds"; # We enter this when we exit the array bounds
        break # We exit the loop
      end;
      # Situated after a continueloop and break, these lines should never execute
      println "This should never be reached";
      exit -1
    done
  end
  
  # Main
  char[] chars = ['A', 'b', 'C', 'd'];
  bool[] bools = [true, true, false, false];
  char[] res = [' ', ' ', ' ', ' '];
  
  # Capitalises characters in chars if the bools flag is set to true
  call zipWith(conditionalToUpper, res, chars, bools);
  call prettyPrint(res)
end
```
Output:
i exceeded array bounds
[A, B, C, d]

## Project Reflection & Final Thoughts

We developed a specification for our own programming language, then wrote our own optimised compiler which produces assembly code directly executable on the ARM11 CPU architecture. The compiler was written entirely in Scala, and we used an open-source Parser-Combinator to handle the parsing of the language developed by our tutor at Imperial College. For version control and code maintenance, we used Git and GitLab’s automated CI/CD pipeline to run an automated test suite, ensuring the functional correctness of our compiler.

We started off developing basic language functionality, first supporting the compilation of functions, loops, if statements and primitive types like strings, integers, void types and hexadecimals. We then added more advanced features like method overloading, higher-order functions, library imports and also exception handling. Finally, we analysed compilation paths and added in compiler optimisations which greatly reduced the number of lines of assembly code that was generated. 

Our compilation process involves 3 main phases: Parsing and Lexical Analysis, which is all about deconstructing the input language into an abstract syntax tree; Semantic Analysis, which handles the error checking to ensure the program is actually valid; and Code Generation, which is all about converting the nodes of the abstract syntax tree into executable ARM11 assembly code.

After this project, I gained a much deeper understanding of functional object orientated programming through the clever features of the Scala language. Gaining an in-depth understanding of the inner-workings of a compiler will certainly be useful in understanding software technologies from a lower level in the future. Additionally, setting up an automated pipeline test to check the correctness of our code with each new commit has greatly saved us time in the long term, and we really appreciated investing the time to set up the CI/CD pipeline and write test cases towards the end of the project.

The project was really successful, and not only did we end up learning so much about Compilers, but we also created completely from scratch our own new programming language capable of handling some pretty advanced programming features like imports, method overloading and higher-order functions.
