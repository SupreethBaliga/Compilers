int main(){
    int a[50]; 
    int n ;
    scanf("%d", &n);
    for(int i = 0 ; i < n ; ++i){
        scanf("%d", &(a[i]));
    }

    for(int i = 0 ; i < n - 1; ++i){
        for(int j = 0 ; j + i + 1 < n ; ++j){
            if(a[j] > a[j + 1]){
                int t = a[j + 1];
                a[j + 1]  = a[j];
                a[j] = t;
            }
        }
    }

    for(int i = 0 ; i < n ; ++i){
        printf("%d ", a[i]);
    }
    printf("\n" , 0);
    return 0;
}