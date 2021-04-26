|'{mm} = 1000.0 '{m}|;


/* constant acceleration formula dx = a * t^2 */ 
float '{m/s*s} func accFormula(float '{m} start1, float '{m} end1, float '{m} start2, float '{m} end2) {
    printf(start1);
    float '{m} dx1 = end1 - start1;    /* dx = 1.5 */
    float '{m} dx2 = end2 - start2;     /* dx = 2.0 */
    float '{s} dt = 0.5;   

    float '{m/s*s} acc = (dx2 - dx1) / (dt * dt);
    printf(acc);
    return acc;
}

int func main() {
    float[] '{mm} s1 = [105.2, 98.4, 100.3];
    float[] '{mm} e1 = [305.8, 309.9, 398.5];
    float[] '{mm} s2 = [105.1, 94.7, 101.1];
    float[] '{mm} e2 = [305.3, 310.1, 399.8];
    float '{cm} a = 100.2;
    int i = 0;
    float '{m/s*s} avg = 0.0;
    for (; i < 3; i = i + 1) {
        avg = avg + accFormula(a, e1[i], s2[i], e2[i]) / 3.0;
    }
    printf(avg);
    return 0;
}