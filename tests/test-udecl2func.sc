int x;
|'{mm} = 1000.0 '{m}|;
|'{km} = 0.001 '{m}|;

float '{mm} p;

float '{km} func foo(float '{km} y) {
    float '{km} x1 = y;
    float '{mm} x2 = 9191.9;
    printf(x2);
    return x1;
}

int func main(){
    float '{km} y = 13.2;
    float '{mm} z = 0.235;
    foo(y);
    return 0;
}