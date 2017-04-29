Group Members
Paul Cruz
Rubens Farias 
Mike Ma

ECE553 Frame Analysis and Intermediate Representation Submission

NOTE: This assignment was due on March 23, 2017. We are submitting 2 days late on March 25, 2017.

Compilation: Compile the project by writing the following commands in the SML window
1) CM.make "sources.cm";
2) Main.compile "TEST_NAME_HERE.tig"

As far as we understand, there are no known bugs to report during the time of submission. We implemented mipsframe.sml, translate.sml, findescape.sml and significantly changed semant.sml during this phase of the project. 

Our findescape.sml module assigns assigns variables and parameters with an escape value of false upon declaration. It then traverses the entire program while keeping track of function depth. When the use of a variable is detected at a different depth, the escape value is then changed to true.

semant.sml was simply changed so that it correctly produces Tree expressions. This was done by simply calling functions from the Translate module and including these function calls as return values.

In translate.sml, a function was created for each of the Absyn variable and expression types. These functions include the logic necessary to translate Absyn expressions into Tree expressions. We also included functions that can be called to produce Tree expressions for things like local variable allocation and declarations. The staticLinking function contains the logic for traversing static links in the frame stack.

mipsframe.sml contains a basic implementation for creating a new frame and allocating space for local variables. These function for allocating a new frame also takes into consideration escapes for variables, thus determining whether or not parameters for functions can be stored in registers or frames.

Lastly, we implemented some array bounds checking by including Tree expressions that check if the access is within the size before reading the memory address and returning the value.

