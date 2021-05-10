/*
Test 12: Linear search in a linked list (Dynamic Memory)
*/

struct Node{
    int val;
    struct Node* nxt;
};

struct Node* insertAtStart(struct Node* head, int x){
    struct Node* tmp = (struct Node*) malloc(sizeof(struct Node));
    tmp->val = x;
    tmp->nxt = head;

    printf("Inserted successfully : %d\n", tmp->val);
    return tmp;
}

void printList(struct Node* head){
    struct Node* p = head;
    while(p){
        printf("%d ", p->val);
        p = p->nxt;
    }
    printf("\n", 0);
}

struct Node* LinSearch(struct Node* head , int elem){
    struct Node* p = head;
    while(p){
        if(p->val == elem){
            return p;
        }
        p = p->nxt;
    }
    return 0;
} 

int main(){
    printf("TC for linear search in a Linked List\n",0);
    struct Node* head = ((void*) 0);
    head = insertAtStart(head, 1);
    head = insertAtStart(head, 2);
    head = insertAtStart(head, 3);
    printList(head);

    struct Node* p = LinSearch(head, 3);
    if(p){
        printf("Found element %d\n" , p->val);
    }else{
        printf("Element not found",0);
    }
    return 0;
}