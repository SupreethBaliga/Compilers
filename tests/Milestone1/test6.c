/********************************* Test ******************************************************************/
/*
* This test program tests structures, union, enumerations, external variables, static variables and pointer operator.
*/

enum week{Mon, Tue, Wed, Thur, Fri, Sat, Sun}; 

int a = 10;

struct Date{
	int time = Mon;
} today;

union Node{
	int a[20];
};

int main(){

	extern int a;

	for(int i=0;i<5;i++){
		static int count = a;
		count ++;
		printf("Value of 'count' is %d while when i is %d\n",count,i);
	}

	struct Date* tom;
	tom = (struct Date*) malloc(sizeof(struct Date)); 
	tom->time = Tue;

	union Node head;
	printf("Memory occupied by union : %d\n", sizeof(head));

	return 0;
}