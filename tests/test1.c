/********************************* Test ******************************************************************/
/*
* This test program tests nested whteher various forms of comments work in the lexer.
* This itself is a block comment
*/
int main() {// A single line comment
// Another single line comment
    printf("Hello World!\n");/* Another block comment
    **
    **

    **
    */ return /*Final comment*/ 0;
}
