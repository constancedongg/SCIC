int func main() {
    print(10 + 19801);          /* Add */ 
    print(0 - 1234567);         /* Sub */ 
    print(100 * 900000);        /* Mul */ 
    print(90 / 80);             /* Div -> floor */ 
    print(-90 / 80);            /* Div -> sign abs(floor(res)) */
    printb(2  > -2);            /* Greater */
    printb(1 + 4 == 5);         /* Equal, Add */
    printb(7 == 30 / 4);       /* Equal, Div */
    printb(12345678901234 >= 12345678901233);      /* Geq */
    print(2147483647);
    print(2147483648);
    printb(2147483647 > 2147483648);      /* Greater */
    printb(-100 <= 51-151);       /* Leq */
    printb(-91 < -1);             /* Less */
    print(-187);                    /* Neg */
    return 0;
}