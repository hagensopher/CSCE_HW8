//The factorial example from the webpage
//note: like in haskell all "if" must have an "else"
//note2: do not use partheneis for while and if stmts (lilke python)
;;;;;
var x = 20;
var acc = 1;
while 1<x {
  acc = acc * x;
  x = x - 1;
}
print "result is ";
print acc;
print "\n";
;;;;;