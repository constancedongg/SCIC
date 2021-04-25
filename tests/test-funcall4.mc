void func foo(int x, int y, bool a, bool b) {
    if (a && b) 
        print(x + y);
    else 
        print(x - y);
}

int func main() {
    foo(1, 2, true, false);   /* -1 */
    foo(1, 2, true, true);    /*  3 */ 
    return 0;
}