/********************************* Test ******************************************************************/
/*
* This test program tests nested if statements. It also tests various operators.
*/
void main() {

	bool ok = 1;
	bool notok = 0;

	if(ok >= 1 && 1){
		if(notok==1 || 0){
			printf("Ok proceed");
		}
		else if(notok!=1){
			printf("Token 'notok' has True value");
		}
		else{
			printf("Critical Error! Bool should be either 0 or 1");
		}
	}
	else if(ok <= 0){
		printf("Token 'ok' has False value");
	}
	else{
		printf("Critical Error! Bool should be either 0 or 1");
	}

	printf("\n");

	long _a1_ = 10, _b2_ = 20;


	_a1_ *= _b2_;
	_a1_ += _b2_;
	_a1_ -= _b2_;
	_a1_ /= _b2_;
	_a1_ %= _b2_;
	_a1_ &= _b2_;
	_a1_ |= _b2_;
	_a1_ ^= _b2_;
	_a1_ <<= 1;
	_a1_ >>= 1;

	_a1_++;
	_a1_--;
	++_a1_;
	--_a1_;


	printf("%d",_a1_);

    
}


