/*
Test 15:Program using library functions (say, sin(x) or cos(x))
*/

float f(float s) {
    float a = sqrt(s);
    printf("%f\n", a);
    a = ceil(s);
    printf("%f\n", a);
    a = floor(s);
    printf("%f\n", a);
    a = fabs(-(s+1.8));
    printf("%f\n", a);
    a = log(s);
    printf("%f\n", a);
    a = log10(s);
    printf("%f\n", a);
    a = fmod(s, 10.0);
    printf("%f\n", a);
    a = exp(s);
    printf("%f\n", a);
    a = cos(s);
    printf("%f\n", a);
    a = sin(s);
    printf("%f\n", a);
    a = acos(-1);
    printf("%f\n", a);
    a = asin(-1);
    printf("%f\n", a);
    a = tan(s);
    printf("%f\n", a);
    a = atan(s);
    printf("%f\n", a);
    return a;
}


int main() {
    printf("TC for displaying math libraries\n",0);
    float s=2.5;
    float m = 7.7;
    float k = f(s+m);
    printf("k = %f\n", k);
    return 0;
}