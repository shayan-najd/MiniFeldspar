/* Philip Wadler, 16 May 2014 */

#include <math.h> 
#include <stdio.h> 

double sqr (double x) {
  return x*x;
}

double test2 () {
  int i = 0;
  double s = 0;
  while (i < 100002) {
    s = s + (i<1 ? 0 : (i<100001 ? sqrt(sqr(i<2 ? 0 : sqrt(sqr(i-2)*sqr(i-1)))
                                     *sqr(i<100000 ? sqrt(sqr(i-1)*sqr(i)) : 0)) : 0));
    i = i+1;
    }
  return s;
}

main () {
  printf("test2: %f\n", test2());
}

/*
[craster]wadler: gcc test2.c -lm
[craster]wadler: a.out
test2: 1999850003999948183437312.000000
[craster]wadler: time a.out
test2: 1999850003999948183437312.000000

real	0m0.011s
user	0m0.010s
sys	0m0.000s
*/
