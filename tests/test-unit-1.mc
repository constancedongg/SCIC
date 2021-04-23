/*
test local var declaration with unit
*/

int '{m} x;
float '{cm} y;
int '{m} func main(){
    x = 2;
    int '{cm} z = 1;
    print(z);
    int '{m} w = x;
    print(x);
    print(w);
    printl("in main");
    return 0;
}