#define ll int

#include <unistd.h>
#include <stdio.h>

void printi(int num) {
  const int bufsize = 21;
  char buf[bufsize];
  buf[bufsize-1] = '\n';
  int i = bufsize-2;
  int neg = 0;
  if (num < 0) {
    num = -num;
    neg = 1;
  }
  do {
    buf[i] = num % 10;
    buf[i] += '0';
    i = i - 1;
    num = num / 10;
  } while(num > 0);
  if (neg) {
    buf[i--] = '-';
  }
  write(1, buf+i+1, bufsize-i-1);
}

int string_length(const char *str) {
  int i = 0;
  while (*(str+i)) ++i;
  return i;
}

ll sum(ll a, ll b) {
  return a + b;
}
