|'{km} = 0.001 '{m}|;
|'{ms} = 1000.0 '{s}|;

/* return value will be auto-converted */
float '{m/s} func foo() {
    float '{cm} x = 250.0;
    float '{s} t = 2.5;
    return x / t;
}

float '{km/s} func bar(float '{cm/ms} a) {
    float '{cm/ms} b = 31.2 ^ 2;
    printf(a);
    printf(b);
    if (a > b) {
        return a + b;
    }
    else {
        return b - a;
    }
}

int func main() {
    printf(foo()); 
    printf(bar(foo()));
    return 0;
}