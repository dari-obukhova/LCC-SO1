FLAGS = -Wall -Wextra -Werror  -g
EJECUTABLE = programa

#-Wall y -Wextra: activan todos las advertencias
#-Werror: convierte las advertencias en errores
#-std=c99: usa el est´andar C99
#-g: genera informaci´on para el debugging

#-c: Compila el codigo sin linkear, se crea un archivo objeto.
#-o: cambia el nombre por defecto del archivo generado por uno elegido.
#-lm: para enlazar la libreria math
#-pthread: POSIX

main : main.o Game.o Board.o Barreras.o
	gcc -o simulador main.o Game.o Board.o Barreras.o -lm -pthread

Barreras.o : Barreras.c Barreras.h
	gcc -c $(FLAGS) Barreras.c

Board.o : Board.c Board.h
	gcc -c $(FLAGS) Board.c -lm

Game.o : Game.c Game.h
	gcc -c $(FLAGS) Game.c -lm -pthread

test01.o : test01.c
	gcc -c $(FLAGS) test01.c

main.o : main.c
	gcc -c $(FLAGS) main.c

test01 : test01.o Game.o Board.o Barreras.o
	gcc -o test01 test01.o Game.o Board.o Barreras.o -lm -pthread

