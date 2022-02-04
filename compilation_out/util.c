#define ll int

#define _XOPEN_SOURCE 500

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <termios.h>

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
  /* fflush(stdout); */
}

void putch(int c) {
  printf("%c", c);
  /* fflush(stdout); */
}

void puti(int i) {
  printf("%d", i);
  /* fflush(stdout); */
}

void sleep_for(useconds_t usec) {
  usleep(usec);
}

void nonblock() {
  int fd = STDIN_FILENO;
  int flags = fcntl(fd, F_GETFL, 0);
  fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

void unbuffer_term() {
  struct termios old_tio, new_tio;
  unsigned char c;

  /* get the terminal settings for stdin */
  tcgetattr(STDIN_FILENO, &old_tio);

  /* we want to keep the old setting to restore them a the end */
  new_tio = old_tio;

  /* disable canonical mode (buffered i/o) and local echo */
  new_tio.c_lflag &= (~ICANON & ~ECHO);

  /* set the new settings immediately */
  tcsetattr(STDIN_FILENO, TCSANOW, &new_tio);
}

char getch() {
  return getchar();
}
