/*
Test 14: Graph as adjacency list/matrix (Dynamic Memory OR MD-Array)
*/

struct node {
    int vertex;
    struct node *next;
};

struct node* new_node(int val) {
    struct node* tmp = (struct node*)malloc(sizeof(struct node));
    tmp -> vertex = val;
    tmp ->next = ((void*)0);
    return tmp;
}

void insert(int from, int to, struct node *adj[10]) {
    for(int i=0;i<10;i++) {
        if (adj[i]->vertex == from) {
            struct node* curr = new_node(to);
            struct node* temp = adj[i]->next;
            adj[i]->next = curr;
            curr->next = temp;
            return;
        }
    }
    printf("Such a node does not exist\n",0);
}

void print_list(struct node *adj[10]) {
    for(int i=0;i<10;i++) {
        printf("Edges from %d:", i);
        struct node* curr = adj[i]->next;
        while(curr != (void*)0) {
            printf("%d ", curr->vertex);
            curr = curr->next;
        }
        printf("\n",0);
    }
    
}

int main() {
    struct node* adj[10];
    for(int i=0;i<10;i++) {
        adj[i] = new_node(i);
    }
    //adding edges
    insert(1,4,adj); insert(2,1,adj); insert(6,7,adj); insert(7,8,adj); insert(8,5,adj);
    insert(1,5,adj); insert(2,7,adj); insert(6,8,adj); insert(7,5,adj); insert(8,0,adj);
    insert(1,9,adj); insert(2,3,adj); insert(6,0,adj); insert(7,1,adj); insert(8,3,adj);
    insert(1,3,adj); insert(2,5,adj); insert(6,8,adj); insert(7,2,adj); insert(8,1,adj);
    insert(1,8,adj); insert(2,6,adj); insert(6,4,adj); insert(7,3,adj); insert(8,6,adj);
    print_list(adj);
    return 0;
}
