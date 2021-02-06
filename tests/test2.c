/*********************** Primitive Test ******************************************************************/
/*
* This test program tests the following tokens apart from the literal symbols:
* Types float, double; Keywords const, return; String literal; Identifiers printf, scanf; Also tests the column
* numbers by use of space and tab
*/

int main() {

    float rad   ;
    const float     pi = 3.14159;
    printf("Enter a value for radius of sphere\n");
    scanf("%f",     &rad);

    double diameter     =   2*rad;
    double surface_area = 4*pi*rad*rad;
    double volume = (4*pi   *   rad*rad*rad)/3;

    printf("The values are as follows:\n Diameter = %lf\n Surface Area = %lf\n Volume = %lf\n", diameter, surface_area, volume);
    return 0;
}
