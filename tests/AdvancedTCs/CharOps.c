/*
This test case is just to cover the character operations
This is kept separate because we had done a lot of hard work in handling operations for chars
and they were not covered in other cases. :-)
*/

struct temp {
    int k;
    char c;
};

struct temp2 {
    char d;
    int x;
    struct temp obj;
};

struct temp2 print(struct temp2 obj) {
    printf("%d\n", obj.x);
    printf("%d\n", obj.obj.k);
    printf("%c\n", obj.d);
    printf("%c\n", obj.obj.c);
    obj.obj.c = 'z';
    obj.d = 'y';
    return obj;
}

int main() {

    printf("TC to demonstrate the use of char operators\n",0);
    // demonstrates that passing structs as well won't change anything
    struct temp2 obj;
    obj.x = 10;
    obj.d = 'a';
    obj.obj.k = 20;
    obj.obj.c = 'b';
    obj = print(obj);
    printf("%d\n", obj.x);
    printf("%d\n", obj.obj.k);
    printf("%c\n", obj.d);
    printf("%c\n", obj.obj.c);
    printf("\n",0);

    // demonstrating all operations with char
    char a = 100, b = 110;
    printf("%c\n", a);
    printf("%c\n", b);
    char a1 = a + b;
    char a2 = a - b;
    char a3 = a * b;
    char a4 = a / b;
    char a5 = a % b; 
    char a6 = -a;
    char a7 = a & b; 
    char a8 = a | b; 
    char a9 = a ^ b; 
    char a10 = !a;
    char a11 = ~a; 
    char a12 = a && b;
    char a13 = a || b;
    printf("a1 = %d\n", a1);
    printf("a2 = %d\n", a2);
    printf("a3 = %d\n", a3);
    printf("a4 = %d\n", a4);
    printf("a5 = %d\n", a5);
    printf("a6 = %d\n", a6);
    printf("a7 = %d\n", a7);
    printf("a8 = %d\n", a8);
    printf("a9 = %d\n", a9);
    printf("a10 = %d\n", a10);
    printf("a11 = %d\n", a11);
    printf("a12 = %d\n", a12);
    printf("a13 = %d\n", a13);
    return 0;
}