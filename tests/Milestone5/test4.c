/*
Test 4: Short Circuit Evaluation of && and ||
*/

int main(){
    printf("Testcase for short circuit evaluation\n", 0);
    int a = 0 , b = 0 , c = 0;
    if(c++ || (a++  && (b++ >= 1))) {
        printf("This would not be printed\n", 0);
    }else{
        printf("a = %d\n" , a);
        printf("b = %d\n" , b);
        printf("c = %d\n" , c);
        if(b ==1  || --a){
            printf("This would not be printed\n", 0);
        }else{
            printf("This would be printed, thanks to short-circuiting\n", 0);
        }
    }
    printf("a = %d\n" , a);
    printf("b = %d\n" , b);
    printf("c = %d\n" , c);
    return 0;
}