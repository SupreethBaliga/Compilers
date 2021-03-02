/********************************* Test ******************************************************************/
/*
* This test program tests arrays, characters, strings. It will also analyze compiler behaviour on a few non intuitive cases.
*/

void main(){
	int _1 = 0x13;
	int arr[8];

	arr[3] = 5;

	char c = 'c', charr[10];

	charr[5] = c + 2;
	charr[5] += 1;

	long _2 = 2, _3 = 3;

	_2 = _2 - --_3;
	_2 = _2 ---_3;
	_2 = _2 --+_3;
	_2 = _2 -++_3;
	_2 = _2 +++_3;
	_2 = _2 + ++_3;



}