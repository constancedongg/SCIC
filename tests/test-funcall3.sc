
float func foo(float x) {
    float a = x + 0.99;
    printf(x);
    printf(a);
    return a / 0.1 * 3.0;
}

int func main() {
    printf(foo(0.1));
    /* assign to a local*/
    float ua = foo(0.2);
    printf(ua); 
    return 0;
}