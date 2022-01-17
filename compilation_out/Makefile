main: main.asm util.c
	nasm -f elf64 -g main.asm -o main.o
	gcc -c -o util.o -ffreestanding -fno-stack-protector util.c
	ld -o main main.o util.o -dynamic-linker /lib64/ld-linux-x86-64.so.2 -lc
