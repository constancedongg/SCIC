
/* example of recording experiment result */
|'{mm} = 1000.0 '{m}|;

void func foo(float '{m/s} base) {
    int i = 0;
    float[] '{m} dx = [2.3, 4.5, 3.4, 0.7];
    float[] '{s} dt = [0.5, 0.2, 1.7, 0.5];
    float[] '{mm/s} res = [0.0, 0.0, 0.0, 0.0];
    for (; i < 4; i = i + 1) {
        res[i] = dx[i] / dt[i] + base ;   /* 2.3m / 0.5s = 4.6 m/s + 0.0122 m/s = 4.6122 m/s = 4612.2 mm/s */
        printf(res[i]);
    }
}

int func main() {
    float '{cm/s} base = 1.22;
    foo(base);                  /* 1.22 cm/s = 0.0122 m/s */
    return 0;
}