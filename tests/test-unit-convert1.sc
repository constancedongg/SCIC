/* test local var declaration with unit */

float '{cm} y;
int func main(){
    float '{m} z = 0.05;
    printf(z); /* 0.05 m*/
    y = z;     /* auto-conversion cm<-m*/
    printf(y); /* 5 cm*/
    printf(z); /* 0.05 m<- z unchanged*/
    return 0;
}