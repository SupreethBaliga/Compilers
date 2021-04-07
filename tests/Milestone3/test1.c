// test for checking for loop and while loops

int func(int a, float b) {
    float c = (a + b)/2;
    int d = (int) c;
    return d;
}

int main() {
    int k,a=0;
    k=10;
    for(int i=0;i<k;i++) {
        a++;
    }

    while(k>=0) {
        int s;
        a++;
        k--;
    }
    return 0;
}