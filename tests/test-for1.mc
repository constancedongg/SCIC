int func main() {
    int[] arr = [1, 3, 5, 7, 5, 3, 1];
    int i = 0;
    for (; i < 7; i = i + 1) {
        print(arr[i]);
        arr[i] = i;
    }

    printl("========");

    for (i = 6; i >= 0; i = i - 1) {
        print(arr[i]);
    }
    return 0;
}