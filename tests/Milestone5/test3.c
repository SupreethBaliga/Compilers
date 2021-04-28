/*
Test 3: Fibonacci
*/


// Iterative version
int main(){
    int a = 0 , b = 1;
    int n;
    scanf("%d",&n);
    printf("Testcase for first %d fibonacci numbers : \n", n);
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
