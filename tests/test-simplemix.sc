/* Unit declaration */
|'{mm} = 1000.0 '{cm}|;

/* Global variable declaration */
float '{m} x;

/* Function declarations */
int func gcd(int a, int b) {
    while(a != b) {
        if(a > b)
            a = a - b;
        else
            b = b - a;
    }
    return a;
}

/* Main */
int func main() {
    print(gcd(12, 32));
    return 0;
}