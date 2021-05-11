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
    printf("TC For BinSearch\n *Input needed*\n",0);
    printf("Input the number of terms to search among (<50 for now): ",0);
    scanf("%d", &n);
    printf("Enter each term of the input in sorted manner:\n",0);
    for(int i = 0 ; i < n ; ++i){
        scanf("%d", &(arr[i]));
    }
    printf("Enter the element to search for from the input\n",0);
    scanf("%d", &elem);

    printf("binSearch() = %d" , binSearch(arr, elem, 0, n -1));
    return 0;
}