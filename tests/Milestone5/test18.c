/*
Test 18: Large expression
*/

int main() {
    printf("TC for large expression\n",0);
    int a=10, b=19, c=21;
    float k = 10.9, f = 17, g = 87.1;

    float ans1 = (a-b%c)/(k*f - g/b)*(pow(g,4)*0.891);
    float ans2 = (b * (a-10.3))*(k/f/g);
    // add more expressions if needed
    
    printf("ans1 = %f\n", ans1);
    printf("ans2 = %f\n", ans2);
    return 0;
}