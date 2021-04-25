|'{km} = 0.001 '{m}|;

/* parameters can be auto-converted when paased into function */
float '{km} func foo(float '{m} x) {     
    float '{km} y = x;
    return y;
}

int func main() {
    float '{cm} tmp = 3000.0;
    printf(foo(tmp)); 
    return 0;
}