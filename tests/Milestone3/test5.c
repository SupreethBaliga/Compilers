struct a {
    int ca;
    int cb;
};

union b {
    int k;
};

int main() {
    int int s;
    a = 2; //should give undeclared error
    int k = func1(d,c); //undeclared error

    struct a* l;
    l -> b = 1; //undeclared error
    struct a obj1;
    union b obj2;
    obj1 = obj2;
}