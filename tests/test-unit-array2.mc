/* test array access */

|'{ms} = 1000.0 '{s}|;

int func main() {
    float[] '{cm} x = [1.1, 2.2, 3.3];
    float[] '{m} y = [1.0, 2.0];
    y[0] = 5.5;
    printf(y[0]);
    y[1] = x[1];
    printf(y[1]);
    return 0;
}