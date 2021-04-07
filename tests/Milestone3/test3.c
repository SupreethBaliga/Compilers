struct a {
    int xa;
    float xb;
    long xc;
};

union b {
    int a;
    long double c;
};

int main() {
    struct a tmp1, tmp2;
    tmp1.xa = tmp1.xb = tmp1.xc = 2;
    tmp2 = tmp1;

    union b obj1, obj2;
    obj1.a = 1;
    obj1.c = 2;
    obj2 = obj1;
    return 0;
}