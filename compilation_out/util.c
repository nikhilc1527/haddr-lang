#define ll int

#define _XOPEN_SOURCE 500

#include <unistd.h>
#include <stdio.h>

/* void printi(int num) { */
/*   const int bufsize = 22; */
/*   char buf[bufsize]; */
/*   buf[bufsize-1] = 0; */
/*   buf[bufsize-2] = '\n'; */
/*   int i = bufsize-3; */
/*   int neg = 0; */
/*   if (num < 0) { */
/*     num = -num; */
/*     neg = 1; */
/*   } */
/*   do { */
/*     buf[i] = num % 10; */
/*     buf[i] += '0'; */
/*     i = i - 1; */
/*     num = num / 10; */
/*   } while(num > 0); */
/*   if (neg) { */
/*     buf[i--] = '-'; */
/*   } */
/*   /\* write(1, buf+i+1, bufsize-i-1); *\/ */
/*   printf("%s", buf+i+1); */
/* } */

void printi(int num) {
  printf("%d\n", num);
  fflush(stdout);
}

void putch(int c) {
  printf("%c", c);
  fflush(stdout);
}

void puti(int i) {
  printf("%d", i);
  fflush(stdout);
}

void sleep_for(useconds_t usec) {
  usleep(usec);
}
