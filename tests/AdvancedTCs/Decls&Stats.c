/*
Mixing of declarations and statements:
ANSI C has the grammar structure that tells you to make all the declarations of the function before writing any operations.
We have changed the grammar to allow the mixing of declarations as well as statements. This allows a lot more flexibility
*/

int main() {
    printf("TC to demonstrate the feature of mixing of declarations and statements\n",0);
    int a, b;
    a = 9 + 67%4;
    b = a*a - a+a;
    printf("a = %d\n",a);
    printf("b = %d\n",b);
    int c = 9;
    c = b/a;
    printf("c = %d\n",c);

    // earlier you could not declare anything inside the for loop condition
    // but now you can :-)

    for(int x=-1, y = 9, z =2; x*y*z !=0; x--,y--,z++){
        printf("x = %d,",x);
        printf("y = %d,",y);
        printf("z = %d,",z);
        printf("x*y*z = %d\n",x*y*z);
    }

}