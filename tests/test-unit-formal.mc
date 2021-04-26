|'{mm} = 1000.0 '{m}|;

void func test (float '{m} start1, float '{m} end1, float '{m} start2, float '{m} end2) {
    printf(start1); /* 3 */
    printf(end1); /* 0.4 */
    printf(start2);  /* 3.01 */
    printf(end2);   /* 0.41 */
} 

int func main() {
    float[] '{mm} s1 = [105.2, 98.4, 100.3];
    float[] '{mm} e1 = [305.8, 309.9, 398.5];
    float[] '{mm} s2 = [105.1, 94.7, 101.1];
    float[] '{mm} e2 = [305.3, 310.1, 399.8];
    
    float '{mm} tmp = 3000.0;
    float '{mm} tmp2 = 400.0;
    test(s1[0], s1[1], s1[2] + 10.0, tmp2 + 10.0); 
    return 0;
}
