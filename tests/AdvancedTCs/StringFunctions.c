/*

TC to display the use of string functions

*/

int main() {
    printf("TC to display the usage of string functions\n\n",0);
    char *init = "Hey this is the beginning. ";
    printf("init: %s\n", init);
    char arr[100];
    arr[0] = 'h';
    arr[1] = 'e';
    arr[2] = 'l';
    arr[3] = 'l';
    arr[4] = 'o';
    arr[5] = '.';
    arr[6] = '\0';
    printf("arr before: %s\n", arr);
    strupr(arr);
    printf("arr after strupr: %s\n", arr);
    strcat(arr, init);
    printf("arr after strcat: %s\n", arr);
    printf("strcmp between arr and init: %d\n", strcmp(arr,init));
    strlwr(arr);
    printf("arr after strlwr: %s\n", arr);
    strupr(arr);
    printf("arr after strupr: %s\n", arr);
    char arr2[100];
    strcpy(arr2, arr);
    printf("arr2: %s\n", arr2);
}