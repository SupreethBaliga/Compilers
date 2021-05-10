/*
Float Ops: This testcase shows all the operations handled for the float datatype
*/

float floatops(float a, float b) {
    float a1 = a + b;
    float a2 = a - b;
    float a3 = a * b;
    float a4 = a / b;
    // float a5 = a % b; // will not work
    float a6 = -a;
    // float a7 = a & b; //will not work
    // float a8 = a | b; //will not work
    // float a9 = a ^ b; //will not work
    // float a11 = ~a; //will not work
    float a10 = !a;
    float a12 = a && b;
    float a13 = a || b;
    printf("a1 = %f\n", a1);
    printf("a2 = %f\n", a2);
    printf("a3 = %f\n", a3);
    printf("a4 = %f\n", a4);
    printf("a6 = %f\n", a6);
    printf("a10 = %f\n", a10);
    printf("a12 = %f\n", a12);
    printf("a13 = %f\n", a13);
    return a4;
}


int main() {
    float a,b;
    scanf("%f", &a);
    scanf("%f", &b);
    float ans = floatops(a,b);
    printf("ans = %f\n", ans);
    return 0;
}