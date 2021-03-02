/*********************** Primitive Test ******************************************************************/
/*
* This test program tests the following tokens apart from the literal symbols:
* Types float, double, bool, int, long; Keywords const, return; String literal; Identifiers printf, scanf; Also tests the column
* numbers by use of space and tab
*/

int main() {

    float rad   ;
    	const float     pi = 3.14159;
    printf("Enter a value for radius of sphere\n");
    scanf("%f",     &rad);

    int _len;
    printf("Enter a value for side of square\n");
    scanf("%d", &_len)	;

    long area_sqaure = _len*_len;
    bool ok=1;


    double diameter     =   2*rad;
    double surface_area_sphere = 4*pi*rad*rad;
    double volume = (4*pi   *   rad*rad*rad)/3;

    printf("The values are as follows:\n Diameter = %lf\n Surface Area = %lf\n Volume = %lf\n", diameter, surface_area_sphere, volume);
    printf("Area of square is %ld\n", area_sqaure)	;
    return 0;
}
