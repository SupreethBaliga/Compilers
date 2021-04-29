/*
Test 16: struct/struct data type as function parameter and return
*/


struct tmp {
    int x;
    float y;
};

int global_count;

struct tmp new_node() {
    struct tmp obj;
    obj.x = pow(global_count, 2);
    obj.y = obj.x/pow(global_count,3);
    global_count ++;
    return obj;
}

void print_node(struct tmp node) {
    printf("Printing the values of node\n",0);
    printf("node.x = %d\n", node.x);
    printf("node.y = %f\n", node.y);
    return;
}

int main() {
    global_count = 1;
    for(int i=0;i<10;i++) {
        struct tmp node = new_node();
        print_node(node);
    }
    return 0;
}