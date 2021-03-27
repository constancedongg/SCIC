int func main () {
    printb(true || false);
    printb(false && true);
    
    printb(true && (true || false));
    printb( false == (true || false));
    
    printb( !false );
    printb( ! (3>1));
    return 0;
}