/*
TC for ternary and switch case
*/

int main(){
    printf("TC for ternary and switch case\n",0);

    int a = 1;
    int b = 2;
    int c = (a&b) ? a++ : b++;
    printf("%d\n", c);
   
    switch (c){
        case 1 : c *=2;
                break;
        case 2 : c *= 3;
        case 3 : c *= 5;
                break;
        default : c *= 7;
    }
    printf("%d\n", c);
}