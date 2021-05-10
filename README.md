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
* To use the compiler execute `smallc` and provide the test file(s) as command line arguments
```bash
bash smallc file1 file2 file3
```
* The available flags can be listed with *-h/--help*
```bash
bash smallc -h
```
* The compiler generates the executable in *executable* folder with the name of the executable being the name of the corresponding file (without the file extension)
# Final Project:
## Directory Structure
* src - Contains all the python code files for
* dot - Contains dot files for AST
* ASTgraphs - Contains AST produced in PDF format
* PDFs - Submission files for the different milestones
* ST - Symbol Table in json format
* TAC - 3 Address Code (Intermediate Language Code) of the corresponding test
* tests - Contains the tests for the different Milestones and the final project


## Features
* ANSI-C was used for providing grammar rules. Some minor modifications were made in order to allow interleaving of declarations and statements, and also to allow declaration in for loop. For example, the following is allowed:
```c
for ( int i = 0; i<5; i++)
```
* Available data types:  `int, char and float`
* The target language is `X86_32 bit` and the syntax used for this assembly is AT&T Syntax. We have used gcc linker to incorporate printf, scanf and some math functions like pow,sin,cos etc.
* scanf and printf both require 2 arguments, the first argument is the string and the second argument is the appropriate variable(or its address in case of scanf). If you only want to print a string then also provide 2 arguments where the second argument can by any valid variable or constant. For example, the following line will print "Hello World":
```c
printf("Hello World",0);
```
* While declaring, arrays need to be of a fixed sizes. Expressions involving constants are not allowed. The following is not allowed:
```c
int a[n];
int a[2+ 3];
```
This is allowed:
```c
int a[5];
```
* Global variables can be declared but not initialized globally. They need to be initialized in a scope
```c
int a = 5; 
/*This is not allowed, the variable 
needs to be initialized in a scope */
```
* Only non negative integer static variables can be declared ( We know this is limiting the power of static but it was either this or remove static so we decided to go with this provision)
* Function declarations are not allowed. Only function definitions are allowed:
```c
int f(int a, int b); /* This is not allowed*/

int f(int a, int b){
    /* do something*/
}
/* This is allowed*/
```
* Global variables of type struct/union are not allowed
* Array initializations in the same line as that of declaration is not allowed
```c
int a[3] = {1,2,3}; /* This is not allowed*/
```
* We are not using gcc for pre processing hence, any `#define` will not work. So, in place of `NULL` use `(void*)0`

## Milestone Breakdown
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
### Milestone - 4:
* To use the parser execute parser.sh and provide the test file(s) as command line arguments
* The corresponding Three Address Code is generated in TAC folder
## Group Members :boy:

| Name | Roll Number |
| ----------- | ------- |
| Aaryan Srivastava | 180007 |
| Supreeth Baliga | 180801 |
| Chinmay Goyal | 180206 |
| Nikhil Agarwal | 180475 |
| Sanchit Agrawal | 180664 | 
