/*
Test 20: Newton-Raphson (ADV: Floating point support)
*/

// GFG
// To find root of f(x) = x^3 – x^2 + 2

float EPSILON;

float func(float x)
{
    return x*x*x - x*x + 2;
}
 
// Derivative of the above function which is 3*x^x - 2*x
float derivFunc(float x)
{
    return 3*x*x - 2*x;
}
 
// Function to find the root
void newtonRaphson(float x)
{
    float h = func(x) / derivFunc(x);
    int itr = 0;
    while (fabs(h) >= EPSILON)
    {
        h = func(x)/derivFunc(x);
  
        // x(i+1) = x(i) - f(x) / f'(x) 
        x = x - h;
        itr++;
    }
    printf("The value of the root is : %f\n", x);
    printf("The number of iterations it took to find the root = %d\n", itr);
}
 
// Driver program to test above
int main()
{
    printf("TC for Newton-Raphson\n",0);
    EPSILON = 0.001;
    float x0 = 100; // Initial values assumed
    newtonRaphson(x0);
    return 0;
}