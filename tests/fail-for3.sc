int func main() {
    for (int i = 0; i < 5; i = i + 1) {         /* declare and assign in for loop expression not allowed */
        print(i);
    }
    return 0;
}