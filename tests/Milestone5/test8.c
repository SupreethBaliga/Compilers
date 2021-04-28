/*
Test 8: Binary search in an array (array + recursion)
*/

int binSearch(int arr[50], int elem, int l , int r){
    if( l > r ){
        return -1;
    }

    int m = l + (r - l) / 2;

    if(arr[m] == elem){
        return m;
    }
    if(arr[m] > elem ){
        return binSearch(arr, elem, l , m - 1);
    }else{
        return binSearch(arr, elem, m + 1 , r);
    }
}

int main(){
    int arr[50];
    int n , elem;
    scanf("%d", &n);
    for(int i = 0 ; i < n ; ++i){
        scanf("%d", &(arr[i]));
    }
    scanf("%d", &elem);

    printf("binSearch() = %d" , binSearch(arr, elem, 0, n -1));
    return 0;
}