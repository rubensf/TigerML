Group Members:
Paul Cruz
Rubens Farias 
Mike Ma

ECE553 Register Allocation Submission


Compilation: Compile the project by writing the following commands in the SML window
1) CM.make "sources.cm";
2) Main.compile "TEST_NAME_HERE.tig"

This will give you a shortened version of the output. In order to print out a more verbose version, use the following commands (please note that the int 3 at the end is needed (this is a print option that determines how much output is printed. You can also use values of 1 or 2):
1) CM.make "sources.cm";
2) Main.compileverb "TEST_NAME_HERE.tig" 3

lastly, we've included several files that can be used for compiling all useful tests. You can use these commands to compile all tests that should output interference graphs (excludes the ones with syntax errors, etc) or compile all 50 tests and export them to a text file

1) sml < good_tests.sml > output.txt
or
2) sml < tests.sml > output.txt

Note that good_tests.sml and tests.sml rely on our directories to work properly, therefore using them may require changing them a bit.



The above compilations will provide you with the printing of the interference graph, as well as printing out MIPS assembly with registers allocated (we note that we are still missing the prologue and epilogue because we have yet to implement the procEntryExit functions). 

During this phase, worked on two main modules: Color and RegAlloc. The Color module takes in instructions and returns an allocation of registers to temps, as well as a list of temps that could not be colored. Spilling/Coalescing were mentioned to be extra credit, therefore we do not handle these cases at the moment. The RegAlloc module uses Color as a submodule, and applies an allocation to a list of instructions after using other modules in order to create the interference graph. However, this module was not used in Main.compile or Main.compileverb because spilling was not implemented, the interference graph is already computed in main.sml, and using RegAlloc to recompute the interference graph would have been redundant. 

