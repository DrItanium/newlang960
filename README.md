This is a code generation and lowering framework that takes in a program
written in lisp syntax that comes from another programming language being the
high level language itself. 

This design is very similar to RTL and llvm bitcode but is actually written in
an expert system instead of emulating one. It will be built up over time.

Inside of the logic directory you'll see several code modules that are
referenced inside of init.clp.

The generator module is used to emit i960 assembly opcodes. 
The intermediate module is used to read in the code that some language
generated (doesn't really matter what at this point). 

This will get larger over time
