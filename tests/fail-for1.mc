int func main() {
    int i = 2;
    for ( ; i; ) {      /* 2nd expression in parentheses should be boolean */
        print(i);
    }
    return 0;
}