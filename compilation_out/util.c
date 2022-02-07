#define ll int

#define _XOPEN_SOURCE 500

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <termios.h>

void printi(long long num) {
  printf("%lld\n", num);
}

void putch(char c) {
  printf("%c", c);
}

void puti(long long i) {
  printf("%lld", i);
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

void flush_out() {
  fflush(stdout);
}
