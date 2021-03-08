void hello(int a){
    int b = 2;
    return;
}

int bye(int b){
    hello(b);
    return b;
}

int main(){
    int a = 6;
    int b = 6;
    printf("Hello");
    float c = 12.34;
    bye(a);
    const volatile enum a {hello}typedef int;
    return 0;
}