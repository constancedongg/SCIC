int func foo(int a, int b) {
    return a + b;
}

int func main() {
    int a = 0;
    int b = 1;
    for (; a + b < 15; a = a + b) {
        print(foo(a, b));
        b = b + 1;
    }
    return 0;
}

/*  a = 0, b = 1, foo(a, b) = 1, b = 2, a = 2
    a = 2, b = 2, foo(a, b) = 4, b = 3, a = 5
    a = 5, b = 3, foo(a, b) = 8, b = 4, a = 9
    a = 9, b = 4, break             */