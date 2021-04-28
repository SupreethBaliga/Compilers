int main(){
    int a = 0 , b = 1;
    printf("Testcase for first 10 fibonacci numbers : \n", 0);
    for(int i = 0 ; i < 10 ; ++i){
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