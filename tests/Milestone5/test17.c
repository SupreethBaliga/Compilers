/*
Test 17: Function with >7 parameters
*/

float func(int a1, float a2, int a3, float a4, int a5, int a6, int a7, float a8, int a9, float a10) {
    return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
}


int main() {
    printf("TC for Function with >7 parameters\n",0);
    float ans = func(4, 2.5, 9.8, 2.8, 19, 5, 10.8, 7.31, 9.8, 10);
    printf("%f\n", ans);
    return 0;
}