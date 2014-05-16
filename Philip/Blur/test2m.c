/* Philip Wadler, 16 May 2014 */

#include <math.h> 
#include <stdio.h> 

double sqr (double x) {
  return x*x;
}

double test2m () {
  int i = 0;
  double a[100001];
  while (i<100001) {
    a[i] = (i<1 ? 0 : (i<100000 ? sqrt(sqr(i-1)*sqr(i)) : 0));
    i = i + 1;
  }
  int j = 0;
  double s = 0;
  while (j < 100002) {
    s = s + (j<1 ? 0 : (j<100001 ? sqrt(sqr(a[j-1])*sqr(a[j])) : 0));
    j = j + 1;
  }
  return s;
}

main () {
  printf("test2m: %f\n", test2m());
}

/*
[craster]wadler: gcc test2m.c -lm
[craster]wadler: time a.out
test2m: 1999850003999948183437312.000000

real	0m0.007s
user	0m0.006s
sys	0m0.000s
*/
