/*
Test 9: Odd and Even (mutual recursion)
*/

// Won't work since function declarations are not there in our grammar
// SORRY :-(

void even(int k) {
    if (k==0) return;
    printf("In even with %d\n",k);
    odd(k-1);
}

void odd(int k) {
    if (k==0) return;
    printf("In odd with %d\n",k);
    even(k-1);
}

int main() {
    int k;
    scanf("%d", &k);
    if (k%2) odd(k);
    else even(k);
    return 0;
}
