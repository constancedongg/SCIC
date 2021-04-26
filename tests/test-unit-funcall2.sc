/* formals with different units */
float '{m/s} func foo(float '{m} dx, float '{s} dt) {     
    return dx / dt;
}

int func main() {
    float '{cm} dx = 300.0;
    printf(foo(dx, 5.0));           /* 300cm = 3m, v = 3m/5s = 0.6 m/s */
    return 0;
}