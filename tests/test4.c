/********************************* Test ******************************************************************/
/*
* This test program tests loop statements, nested loop statements, switch statements, jump statements and the corrseponding labels.
*/

int main(){

	long val = 0;
	long int val_2 = 100;

	for(int i=0;i<5;i++){
		for(int j=0;j<5;j++){
			if(j==0) continue;
			val++;
		}
	}

	do{
		val_2 -= 5;
	} while(val--)

	abc:

	switch(val_2){
		case 0: val = 0;
			break;
		case 1: val -= 1;
			break;
		default: val = val/2;
	}

	if(val){
		goto abc;
	}

	return 0;
}