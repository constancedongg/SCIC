|'{km} = 0.001 '{m}|;

int func main() {
    float '{m} start1 = 1.05;
    float '{m} end1 = 2.05;
    float '{m} dx1 = end1 - start1;

    float '{m} start2 = 2.05;
    float '{m} end2 = 4.05;
    float '{m} dx2 = end2 - start2;

    float '{s} start_t = 2.0;
    float '{s} end_t = 0.0;
    float '{s} dt = end_t - start_t; 
    float '{km/s*s} a = (dx2 - dx1) / (dt * dt);
    printf(a);
    return 0;
}