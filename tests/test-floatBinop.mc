int func main() {
    printf(1.2 - 0.1);          /* Sub */
    printf(1.2 + 2.876);        /* Add */ 
    printf(1.2 - 2.876);        /* Sub */ 
    printf(1.2 * 2.876);        /* Mul */ 
    printf(1.2 / 2.4);          /* Div */ 
    printf(1.2 / 2.874);        /* Div, truncation at the 6th digit (in total) */
    printb(2.1234567 > 1.5);    /* Greater */
    printb(1.0 == 1.0);         /* Equal - not necessarily correct? */
    printb(1.00 == 1.0);        /* Equal - not necessarily correct? */
    printb(0.99 >= 1.000);      /* Geq */
    printf(2.0 ^ 3.0);         /* Pow float ^ float*/
    printf(2.0 ^ 1.5);         /* Pow float ^ float */
    printf(3.0 ^ 2);         /* Pow float ^ int */
    printf(5.5 ^ 3);            /* Pow float ^ int */
    return 0;
}