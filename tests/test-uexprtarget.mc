/*
test derived unit
*/
|'{g} = 1000.0 '{kg}|;
float '{m*g} x;
float '{cm*kg} y;
int func main() {
    printl("in main");
    x = 9.98;
    y = x;
    printf(y);
    return 0;
}