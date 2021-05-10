/*

Static: Test Case for displaying the use of static variables

Note: Only static ints are allowed. And if initialising along with declaration, only positive integers allowed
*/

void func() {
    int k = 9;
    k++;
    static int s = 9;
    s++;
    printf("Value of k = %d\n", k);
    printf("Value of s = %d\n", s);
    return;
}

int main() {
    int i = 5;
    while(i--) {
        func();
    }
    return 0;
}
