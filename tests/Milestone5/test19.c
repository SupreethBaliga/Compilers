/*
Test 19: File I/O
*/

// 1. File I/O bought in a lot of issues since there is no direct library to handle function calls.
// 2. adding a new data_type required to make a lot of changes in type checking which was not feasible
// 3. We need to maintain a struct type data structure to maintain all the meta data of the file. 
//    This again looked infeasible in the current implementation

// TC Credits: tutorialspoint.com
int main() {
    printf("TC for file pointer\n",0);
    FILE *fp;
    fp = fopen("/tmp/test.txt", "w+");
    fprintf(fp, "This is testing for fprintf...\n");
    fputs("This is testing for fputs...\n", fp);
    fclose(fp);
}

