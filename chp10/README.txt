Group Members
Paul Cruz
Rubens Farias 
Mike Ma

ECE553 Instruction Selection Submission


Compilation: Compile the project by writing the following commands in the SML window
1) CM.make "sources.cm";
2) Main.compile "TEST_NAME_HERE.tig"

This will produce two series of printed statements. The first series consists of Tree expressions, which represent the state of the frags AFTER using the Canon module on the frags produced by our semantic analysis. The second series of statements is the assembly code that is produced by our MipsGen module (note that this is still not valid MIPS as we have yet to finish register allocation).

As far as we understand, there are no known bugs to report during the time of submission. We implemented mipsframe.sml, translate.sml, findescape.sml and significantly changed semant.sml during this phase of the project. 

Our project uses the maximal munch approach that is described in the textbook. The relevant files for this phase include MipsGen and Main. There were also some small changes made to translate.sml and semant.sml in order to get the compilation working.



