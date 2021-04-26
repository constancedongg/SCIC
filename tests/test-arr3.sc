int func main() {
    int x = 5;
    int[] arr = [2 * x, x * x, (10 - 3) * 2, 5];
    int i = 0;
    for (; i < 4; i = i + 1) {
        print(arr[i]);
    }
    return 0;
}