/* From : https://www.geeksforgeeks.org/quick-sort/ */

void swap(int* a, int* b) 
{ 
    int t = *a; 
    *a = *b; 
    *b = t; 
} 

int partition (int arr[], int low, int high) 
{ 
    int pivot = arr[high]; 
    int i = (low - 1);
  
    for (int j = low; j <= high - 1; j++) 
    { 
        if (arr[j] < pivot) 
        { 
            i++;
            swap(&arr[i], &arr[j]); 
        } 
    } 
    swap(&arr[i + 1], &arr[high]); 
    return (i + 1); 
} 

void quickSort(int arr[], int low, int high) 
{ 
    if (low < high) 
    { 
        int pivot = partition(arr, low, high); 
        quickSort(arr, low, pivot - 1); 
        quickSort(arr, pivot + 1, high); 
    } 
} 

int main(){
    int a[50]; 
    int n ;
    scanf("%d", &n);
    for(int i = 0 ; i < n ; ++i){
        scanf("%d", &(a[i]));
    }

    quickSort(a, 0 , n - 1);

    for(int i = 0 ; i < n ; ++i){
        printf("%d ", a[i]);
    }
    printf("\n", 0);
    return 0;
}