int func foo() {
    {
        int x = 2;
        {
            int y = 3;
            print(x + y);
            {
                x = 10;
                print(x - y);
            }
        }
    }
    return 1;
}

int func main() {
    foo();
    return 0;
}