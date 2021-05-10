/*
Test 13: Binary tree traversal (Dynamic Memory) 
We have created BST for simplicity of understanding
*/

struct BTNode{
    struct BTNode *left, *right;
    int val;
};

struct BTNode* insert(struct BTNode *root, int val) {
    if (root == ((void*)0)) {
        root = (struct BTNode *)malloc(sizeof(struct BTNode));
        root-> val = val;
        root->left = ((void*)0);
        root->right = ((void*)0);
        return root;
    }
    else if (val > root->val) root-> right = insert(root->right, val);
    else root->left = insert(root->left, val);
    return root;
}

void inorder(struct BTNode* node) {
    if (node == ((void*)0)) {
        return;
    }
    inorder(node->left);
    printf("%d ", node->val);
    inorder(node->right);
}

int main() {
    printf("TC for binary tree traversal\n",0);
    struct BTNode* root = (void*)0;
    root = insert(root, 10);
    root = insert(root, 11);
    root = insert(root, 545); 
    root = insert(root, 23); 
    root = insert(root, 15); 
    root = insert(root, 68); 
    root = insert(root, 108); 
    root = insert(root, 1); 
    root = insert(root, 76); 
    root = insert(root, 65); 
    root = insert(root, 94); 
    root = insert(root, 11); 
    root = insert(root, 15); 
    inorder(root);
    return 0;
}

