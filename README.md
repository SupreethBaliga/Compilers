# Compiler Project

**Source Language :-** `C`
**Implementation Language:-** `Python 3`
**Target Language:-** `X86_32 bit`

## Instructions:
* Install all the python modules and set up virtual environment by executing install.sh
* If the above step gives an error, repeat the above step
* Activate the virtual environment by running: 
```bash
source venvcompiler/bin/activate
```
### Milestone - 1:
* To use the lexer execute lex.sh and provide the test file(s) as command line arguments

### Milestone - 2:
* To use the parser execute parser.sh and provide the test file(s) as command line arguments
* The available flags can be listed with *-h/--help* (We have added the -l flag which displays the lexer token table)
* Note that we have already added the corresponding dot files and ps files for the provided non-trivial testcases

### Milestone - 3:
* To use the parser execute parser.sh and provide the test file(s) as command line arguments
* The available flags can be listed with *-h/--help* (We have added the -l flag which displays the lexer token table)
* The corresponding dot files go in dot folder, the correspoing AST go in ASTgraphs, the symbol tables go in ST folder

# Final Project:
## Directory Structure
* src - Contains all the python code files for
* dot - Contains dot files for AST
* ASTgraphs - Contains AST produced in PDF format
* PDFs - Submission files for the different milestones
* ST - Symbol Table in json format
* TAC - 3 Address Code (Intermediate Language Code) of the corresponding test
* tests - Contains the tests for the different Milestones


## Features
* ANSI-C was used for providing grammar rules. Some minor modifications were made in order to allow interleaving of declarations and statements, and also to allow declaration in for loop. For example, the following is allowed:
```c
for ( int i = 0; i<5; i++)
```
* The target language is `X86_32 bit` and the syntax used for this assembly is AT&T Syntax. We have used gcc linker to incorporate printf, scanf and some math functions like pow,sin,cos etc.
* scanf and printf both require 2 arguments, the first argument is the string and the second argument is the appropriate variable(or its address in case of scanf). If you only want to print a string then also provide 2 arguments where the second argument can by any valid variable or constant. For example, the following line will print "Hello World":
```c
printf("Hello World",0);
```
* While declaring, arrays need to be of fixed sizes. The following is not allowed:
```c
int a[n];
```


## Group Members :boy:

| Name | Roll Number |
| ----------- | ------- |
| Aaryan Srivastava | 180007 |
| Supreeth Baliga | 180801 |
| Chinmay Goyal | 180206 |
| Nikhil Agarwal | 180475 |
| Sanchit Agrawal | 180664 | 
