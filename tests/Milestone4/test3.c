void swap(int* a,int* b){
    int val1 = *a;
    int val2 = *b;
    *a = val2;
    *b = val1;
}

int main(){
    int a = 4;
    int b = 6;
    swap(&a,&b);
}