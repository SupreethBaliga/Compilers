int main(){
    int a = -1 - 2;
    int b = -1 - a;
    int c = -a - 1;
    int d = -a - b;
    printf("Testcase to check unary minus vs binary minus operateors\n", 0);
    printf("a = -1 - 2 = %d\n", a);
    printf("b = -1 - a = %d\n", b);
    printf("c = -a - 1 = %d\n", c);
    printf("d = -a - b = %d\n", d);
    return 0;
}