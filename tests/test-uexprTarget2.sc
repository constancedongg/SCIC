|'{g} = 1000.0 '{kg}|;
|'{mm} = 10.0 '{cm}|;

float '{mm/g} x;
float '{cm/kg} y;

int func main() {
    printl("in main");
    x = 9.978;
    printf(x);
    y = x - 5.973;
    printf(y);
    float '{m/kg} z = (x - 5.974) * 0.1;
    printf(z);
    float '{m/kg} w = y - z;
    printf(w);
    return 0;
}