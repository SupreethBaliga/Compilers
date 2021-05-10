/*
This is to depict the use of scopes
*/
int b;

int main() {
    printf("TC to demonstrate the use of multi scopes\n",0);
    b = 10;
    int b;
    b = 19; // move this line above to show that local b is being used
    printf("%d\n", b);
    {
        float b;
        b = 9.5;
        printf("%f\n", b);
    }
    {
        printf("%d\n", b);
    }
    {
        struct temp {
            int a;
        };
        struct temp obj; //this is fine
    }
    // struct temp obj2; //this is not fine
}