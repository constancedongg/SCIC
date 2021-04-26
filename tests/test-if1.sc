void func check(int num) {
    if (num < 0)
        printl("negative");
    else
        printl("non-negative");
}

int func main() {
    int x = 5;
    check(x);
    return 0;
}
