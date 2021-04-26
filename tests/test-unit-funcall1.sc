|'{km} = 0.001 '{m}|;

/* parameters can be auto-converted 
    when passed into function */
float '{km} func foo(float '{m} x) { 
    printf(x); /* 30 (m) */    
    float '{m} y = x;
    return y;   /* at return: convert to return unit km */
}

int func main() {
    float '{cm} tmp = 3000.0;
    printf(foo(tmp));  /* 0.03 (km)*/
    return 0;
}