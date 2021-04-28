int main(){
    printf("Testcase for short circuit evaluation\n", 0);
    int a = 0 , b = 4 , c = 3;
    if( (a >= b) && (b++ >= c)){
        printf("This would not be printed\n", 0);
    }else{
        if(b != 5 || (a++)){
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