/* example of recording experiment result */
int func main() {
    int i = 0;
    float[] '{m} dx = [2.3, 4.5, 3.4, 0.7];
    float[] '{s} dt = [0.5, 0.2, 1.7, 0.5];
    float[] '{m/s} res = [0.0, 0.0, 0.0, 0.0];
    for (; i < 4; i = i + 1) {
        res[i] = dx[i] / dt[i];
        printf(res[i]);
    }
    return 0;
}