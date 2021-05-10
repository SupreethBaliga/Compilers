/*
Test 3: Fibonacci
*/


// Iterative version
int main(){
    int a = 0 , b = 1;
    int n;
    printf("TC for Fibonacci Numbers\n *INPUT NEEDED*\n",0);
    printf("Input the number of Fibonacci Terms: ",0);
    scanf("%d",&n);
    for(int i = 0 ; i < n ; ++i){
        if(i < 2){
            if(i == 0){
                printf("%d ", a);
            }else{
                printf("%d ", b);
            }
        }else{
            int c = a + b;
            a = b;
            b = c;            
            printf("%d ", c);
        }
    }
    printf("\n", 0);
    return 0;
}
