float f1(float b, int a){
	return 2*b;
}

int f2(int a){
	return a>>1;
}

struct Date{
	int time;
};


void main(){
	static int x = 1;
	const long int y = 0;

	x = f2(x);
	float z = f1(0.0, x);

	int *ptr;

	ptr = &x;

	printf("%d is stored at address %Ld",*ptr, ptr); 

	struct Date tom;
	tom.time = 0;

	int a, b[2];

	a = sizeof(int(int*));


}
