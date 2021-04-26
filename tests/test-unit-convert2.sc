/* constant acceleration formula dx = at^2 */ 
int func main() {
    float '{m} start1 = 1.05;
    float '{m} end1 = 2.05;
    float '{m} dx1 = end1 - start1;    /* dx = 1.0 */
             
    float '{m} start2 = 2.05;
    float '{m} end2 = 4.05;           
    float '{m} dx2 = end2 - start2;     /* dx = 2.0 */

    float '{s} dt = 0.5;   

    float '{m/s*s} acceleration = (dx2 - dx1) / (dt * dt);
    printf(acceleration);
    return 0;
}