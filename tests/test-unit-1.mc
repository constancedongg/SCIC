/*
test local var declaration with unit
*/

float '{m} x;
float '{cm} y;
int func main(){
    x = 2.0;
    float '{cm} z = 1.0;
    printf(z);
    float '{m} w = x;
    printf(x);
    printf(w);
    printl("in main");
    return 0;
}