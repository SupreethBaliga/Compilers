int main(){

	;;

	int i = 0, j = 0;

	for(i=0; i<10;i++){

	}
	while(j<10){
		j++;
	}

	abc:



// Dangling if-else problem. The code alignment suggests the else combines with the first if
// statement but the parser will parse it correctly and combine it with the second if statement
	if(i>j)
		if(5 > 6)
			i = 1;
	else
		i = 2;


	switch(i){
		case 1: j = 1 ;
		break;
		case 2: j = 2 ;
		break;
		default: j = 3 ;
		break;
	}

	do{
		i++;
	} while(i<20);

	if(i<10){
		goto abc;
	}



	return 0;
}