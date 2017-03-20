Group Members
Paul Cruz
Rubens Farias 
Mike Ma

ECE553 Semantic Analysis Submission

In order to use our semantic analysis module, please compile the entire project with CM.make "sources.cm". You can then run the module on a test file using Main.parse "TESTFILE.tig"

As far as we understand, there are no known bugs to report during the time of submission.

The only files we have edited during this project included env.sml, semant.sml, main.sml, parse.sml, sources.cm, and types.sml. semant.sml contains our implementation for the semantic analysis, which follows recommendations from the textbook. Similarly, env.sml was completed using provided example code from the textbook. main.sml consists of our module for actually running the semantic analysis and viewing compilations/type checking errors. parse.sml, sources.cm, and types.sml were mostly edited in order to fixed small compilation issues regarding the starter code.

The semantic analysis is implemented essentially exactly the way the book recommends, therefore instead of going through all the details, we will only focus on some significant nuances/design choices that were made for the module. 

recursive functions/types: The module type checks these by first going through the type/function headers and adding T.NAME types to the symbol table for these types. The details of each T.NAME is filled in later after each mutually recursive type/function has been added to the table so that they can be looked up during processing. This is done in the TransDec function of the module. Furthermore, it is during this phase that we check for cycles within type definitions.

loop break points: we track break expressions with a var called nest, which increments and decrements based on the depth of the loop.

main.sml: this module functions simply by using our previous parser to produce the abstract syntax tree, which is then passed to the semant.sml module for analysis.