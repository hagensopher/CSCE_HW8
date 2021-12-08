// Here is the fib code from hw7
var x = 0;
var y = 1;
var inp = 6;
var count = 0; 
var temp = 0;
if inp<0 {
    print "Invalid input please try again\n";
} else {
    while count<inp {
        //print x;
        //print "\n";
        temp = x + y;
        x = y;
        y = temp;
        count = count + 1;
    }
}
print x;
print "\n";
