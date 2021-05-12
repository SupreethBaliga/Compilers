/*
Test 2: Operators Type Checking
*/


int main() {
    printf("This TC for displaying the type checking of operators\n",0);
    int a = 5;
    int b = 9;
    float k = 9.6;
    float s = 10.9;
    char c = 'k';
    char d = 'l';
    
    int i1 = a + b; // should work
    float i2 = k + a; //should work
    float i3 = k - s; // should work
    int i4  = c + a; // should work
    char i5 = c + a; // should work
    char i6 = k +a; // should work

    //similar for -, *, /, +=, -=, *=, /=

    // for %
    int i7 = a%b; //should work
    int i8 = a%c; //should work
    int i9a = a%(int)k; //should work (because typecasted)

    int i9b = a%k; //should not work
    float i10 = k%s; // should not work
    int i11 = d%c; //should work
    char i12 = d%c; //should work
    // similar for %=

    // for <<
    int i13 = a<<b; //should work
    int i14 = a<<c; //should work
    int i15 = a<<k; //should not work
    float i16 = k<<s; // should not work
    int i17 = d<<c; //should work
    char i18 = d<<c; //should work
    //similar for <<=, >>, >>=

    // for &
    int i27 = a&b; //should work
    int i28 = a&c; //should work
    int i29 = a&k; //should not work
    float i30 = k&s; // should not work
    int i31 = d&c; //should work
    char i32 = d&c; //should work
    //similar for &=, |, |=  , ^, ^=  

    // for &&
    int i19 = a&&b; //should work
    int i20 = a&&c; //should work
    int i21 = a&&k; //should work
    float i22 = k&&s; // should work
    int i23 = d&&c; //should work
    char i24 = d&&c; //should work
    //similar for ||    

    // for ~ and !
    float i25 = ~k; //should not work
    float i26 = !k; //should work
}