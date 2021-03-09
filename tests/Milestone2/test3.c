enum week{Mon, Tue, Wed, Thur, Fri, Sat, Sun}; 

int a = 10;

struct Date{
	int time;
} today;

union Node{
	int a[20];
};

int main(){

	extern int a;

	int i = 0;

	for(i=0;i<5;i++){
		static int count = 3;
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