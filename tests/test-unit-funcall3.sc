/* return value will be auto-converted */
float '{m} func foo() {
    float '{cm} x = 250.0;
    return x;
}

int func main() {
    printf(foo()); 
    return 0;
}