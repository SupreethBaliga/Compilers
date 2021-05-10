/*

Multiple Level of Pointers in C

This TC displays the same for all datatypes including structs
*/

struct innermost {
    int a;
    float b;
};

struct inner1{
    int a;
    float b;
    struct innermost obj;
};

struct inner2{
    char c;
    float d;
    struct innermost* ptrobj;
};

struct outer{
    char z;
    struct inner1 obj;
    struct inner2* ptrobj;
};

int main() {

    printf("TC to demonstrate the use of multi level pointers\n",0);
    //for native data types
    float a, *aptr, **aaptr, b;
    a = 9.8; b =10.9;
    aptr = &a;
    aaptr = &aptr;
    printf("Value of a = %f\n",a); 
    *aptr = 10;
    printf("Value of a = %f\n",a); 
    **aaptr = 100;
    printf("Value of a = %f\n",a); 
    *aaptr = &b;
    printf("Value of *aptr = %f\n", *aptr); 
    printf("\n",0);
    // for user-defined data types
    struct innermost inobj;
    inobj.a = 1; inobj.b = 0.3333;

    struct inner1 obj1;
    obj1.a = 10; obj1.b = 3.333; obj1.obj = inobj;

    struct inner2 obj2;
    obj2.c = 'a'; obj2.d = 33.33; obj2.ptrobj = &inobj;

    struct outer* outobj = (struct outer*)malloc(sizeof(struct outer));
    outobj -> z = 'b';
    outobj -> obj = obj1;
    outobj -> ptrobj = &obj2;

    printf("z of outer is %c\n", outobj->z);
    printf("a of inner1 is %d\n", outobj->obj.a);
    printf("b of inner1 is %f\n", outobj->obj.b);
    printf("c of inner2 is %c\n", outobj->ptrobj->c);
    printf("d of inner2 is %f\n", outobj->ptrobj->d);
    printf("\n",0);
    printf("a of innermost via inner1 is %d\n", outobj->obj.obj.a);
    printf("b of innermost via inner1 is %f\n", outobj->obj.obj.b);
    printf("\n",0);
    printf("a of innermost via inner2 is %d\n", outobj->ptrobj->ptrobj->a);
    printf("a of innermost via inner2 is %f\n", outobj->ptrobj->ptrobj->b);
}
