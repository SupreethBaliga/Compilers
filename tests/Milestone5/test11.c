/*
Test 11: Matrix Multiplication (Multi-dimensional arrays)
*/

int main(){
    int n;
    int a[10][10] , b[10][10]; 
    printf("TC for Matrix Multiplication\n *INPUT NEEDED*\n",0);
    printf("Enter the dimension of the matrix (n x n) (n<10 for now)\n",0);
    scanf("%d", &n);

    printf("Enter each term of the input of the first matrix in a row-wise form\n",0);
    for(int i = 0 ; i < n ; ++i){
        for(int j = 0 ; j < n ; ++j){
            scanf("%d" , &(a[i][j]));
        }
    }

    printf("Enter each term of the input of the second matrix in a row-wise form\n",0);

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


    printf("The resultant matrix is\n",0);
    for(int i = 0 ; i < n ; ++i){
        for(int j = 0 ; j < n ; ++j){
            printf("%d " , res[i][j]);
        }
        printf("\n",0);
    }

    return 0;
}