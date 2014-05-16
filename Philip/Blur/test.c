/* Philip Wadler, 16 May 2014 */

#include <math.h> 
#include <stdio.h> 

double sqr (double x) {
  return x*x;
}

double test () {
  int i = 0;
  double s = 0;
  while (i<100001) {
    s = s + (i<1 ? 0 : (i<100000 ? sqrt(sqr(i-1)*sqr(i)) : 0));
    i = i + 1;
  }
  return s;
}

main () {
  printf("test: %f\n", test());
}

/*
[craster]wadler: gcc test.c -lm
[craster]wadler: time a.out
test: 333323333400000.000000

real	0m0.005s
user	0m0.004s
sys	0m0.001s
*/


