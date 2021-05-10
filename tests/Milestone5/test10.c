/*
Test 10: Bubblesort (array + nested loop)
*/

int main(){
    printf("TC For bubblesort\n *Input needed*\n",0);
    int a[50]; 
    int n ;
    printf("Input the number of terms to sort (<50 for now): ",0);
    scanf("%d", &n);
    printf("Enter each term of the input:\n",0);
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

    printf("Sorted output is:\n",0);
    for(int i = 0 ; i < n ; ++i){
        printf("%d ", a[i]);
    }
    printf("\n" , 0);
    return 0;
}