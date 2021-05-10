/*
Test 5: Nested Ifs => avoid multiple labels (Backpatching)
*/


int main() {
    printf("TC for nested ifs\n",0);
    int a=9, b=10, c=11;
    if (a==11) {
        printf("This won't be printed\n",0);
    }
    else if(b ==11) {
        printf("This too won't be printed\n",0);
    }
    else if(c==11) {
        printf("Inside the else if for c.\n",0);
        if (a==10) {
            printf("This too won't be printed\n",0);
        }
        else {
            if (b==10) {
                if (a==9) {
                    printf("Wow. I reached the destination\n",0);
                }
                else {
                    printf("This won't be printed\n",0);
                }
            }
            else {
                printf("This won't be printed\n",0);
            }
        }
    }
    else {
        printf("This too won't be printed\n",0);
    }
    return 0;
}