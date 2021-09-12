# WACC Compiler

> This compiler was written as a part of the second year undergraduate computing compilers project by Codrin Cotarlan
> (@cc1619), Daria Petca (@dp1119), Gavin Wu (@gjw19) and Sudarshan Sreeram (@ss8119). Note that this software is
> provided without warranty of any kind; the developers are not held liable for any damages in through use of this code.

The project follows the standard SBT structure; the build configuration file `build.sbt` is present in the root folder.
If using IntelliJ IDEA, open the project from that file. To compile the project, run `make` in the root directory; this
calls sbt to compile and assemble the project. To run the project, invoke the `./compile` command with the required
arguments (`.wacc` files).

Example Program:
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
