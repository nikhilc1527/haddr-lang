main: main.asm util.c
	nasm -f elf64 -g main.asm -o main.o
	gcc -c -g util.c -o util.o
	gcc -g -fsanitize=address,undefined main.o util.o -o main
