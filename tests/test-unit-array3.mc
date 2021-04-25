/* test array access */

|'{ms} = 1000.0 '{s}|;

int func main() {
    float[] '{cm} x = [1.1, 2.2, 3.3];
    float '{m} y = 4.4;
    y = x[1]; 
    printf(y); /*2.2cm = 0.022m, prints 0.022*/
    return 0;
}