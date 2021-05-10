/*
Test 1: Unary Minus vs Binary Minus
*/

int main(){
    printf("This test is for checking Unary Minus vs Binary Minus\n",0);
    int a = -1 - 2;
    int b = -1 - a;
    int c = -a - 1;
    int d = -a - b;
    int e = a - -b;
    int f = -a - -b;
    printf("a = -1 - 2 = %d\n", a);
    printf("b = -1 - a = %d\n", b);
    printf("c = -a - 1 = %d\n", c);
    printf("d = -a - b = %d\n", d);
    printf("e = a -- b = %d\n", e);
    printf("f = -a -- b = %d\n", f);
    return 0;
}

// Success
/*
Expected Values:
a = -3
b = 2
c = 2
d = 1
e = -1
f = 5
*/