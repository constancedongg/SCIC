|'{us} = 1.0E+6 '{s}|;
float '{us} ua;

float '{s} func foo(float x) {
    float '{s} a = x + 0.99;
    printf(x);
    printf(a);
    return a;
}

int func main() {
    printf(foo(0.1));
    /* assign to a global*/
    ua = foo(0.2);
    printf(ua);
    return 0;
}