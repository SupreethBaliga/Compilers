int main(){
    int n;
    int a[10][10] , b[10][10]; 
    scanf("%d", &n);
    for(int i = 0 ; i < n ; ++i){
        for(int j = 0 ; j < n ; ++j){
            scanf("%d" , &(a[i][j]));
        }
    }
    for(int i = 0 ; i < n ; ++i){
        for(int j = 0 ; j < n ; ++j){
            scanf("%d" , &(b[i][j]));
        }
    }

    int res[10][10];

    for(int i = 0 ; i < n ; ++i){
        for(int j = 0 ; j < n ; ++j){
            res[i][j] = 0;
            for(int k = 0 ; k < n ; ++k){
                res[i][j] += a[i][k] * b[k][j];
            }
        }
    }

    for(int i = 0 ; i < n ; ++i){
        for(int j = 0 ; j < n ; ++j){
            printf("%d " , res[i][j]);
        }
        printf("\n");
    }

    return 0;
}