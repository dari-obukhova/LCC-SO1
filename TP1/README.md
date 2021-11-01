# Juego de la vida de Conway

Primer trabajo práctico de **Sistemas Operativos I**

## ALUMNOS:
+ [Joaquin Caporalini](https://git.dcc.fceia.unr.edu.ar/jcaporalini)    e-mail: joaquicaporalini@gmail.com
+ [Daria Obukhova](https://git.dcc.fceia.unr.edu.ar/dobukhova)  e-mail: dari.obukhova@gmail.com

# Desafío

 El [Juego de la Vida
de Conway](https://es.wikipedia.org/wiki/Juego_de_la_vida) es un juego de cero jugadores, lo que quiere decir que su evolución está determinada por el estado inicial y no necesita ninguna entrada de datos posterior. El "tablero de juego" es una malla plana formada por cuadrados (las "células") que se extiende por el infinito en todas las direcciones. Por tanto, cada célula tiene 8 células "vecinas", que son las que están próximas a ella, incluidas las diagonales. Las células tienen dos estados: están "vivas" o "muertas" (o "encendidas" y "apagadas"). El estado de las células evoluciona a lo largo de unidades de tiempo discretas (se podría decir que por turnos). El estado de todas las células se tiene en cuenta para calcular el estado de las mismas al turno siguiente. Todas las células se actualizan simultáneamente en cada turno, siguiendo estas reglas:

+ Una célula muerta con exactamente 3 células vecinas vivas "nace" (es decir, al turno siguiente estará viva).

+ Una célula viva con 2 o 3 células vecinas vivas sigue viva, en otro caso muere (por "soledad" o "superpoblación").

Nuestro desafío es realizar una implementación de este juego de manera concurrente.

# Detalles de la implementación

## Esqueleto Estructural:
El esqueleto del programa realizado está basado en dos cabeceras propuestas por la cátedra, como lo son [Board.h](./Board.h) y [Game.h](./Game.h), a las cuales se les han realizado una serie de modificaciones.

* Game.h: En esta cabecera se encuentran las funciones para jugar. Aquí se encuentran funciones de lectura y escritura de archivos, que nos permiten recuperar los juegos y una función que nos permite jugar. También una función que libera la memoria que se solicitó.

* Board.h: Esta es una librería mucho más aburrida que la anterior. En esta no hay ningún tipo de concurrencia. Es solo de operaciones dentro de la matriz de _Char_ que se usa de tableros.

### Cambios en las cabeceras dadas:

Primero veremos los cambios en [Game.h](./Game.h):

1. El primer cambio fue:
```c
 enum State {ALIVE, DEAD};
```
Por:
```c
enum State {ALIVE = 79, DEAD = 88};
 ```
Esto con el objetivo de poder tener los caracteres O (79) y X (88) que son nuestra representación de células vivas y muertas, los números son los códigos ascii de esas letras.

2. El siguiente cambio es:
```c
void writeBoard(board_t board, const char *filename);
```
por
```c
void writeBoard(board_t * board, const char *filename);
```
Esto se realizó con el objetivo de simplificar el código para nosotros.

3. El último cambio dentro de los destacados en esta cabecera fue:
```c
board_t *congwayGoL
  (board_t *board, unsigned int cycles, const int nuproc);
```
Que fue reemplazado por
```C
board_t *congwayGoL(game_t * game);
```
  Esto se hizo pues toda esa Información ya se encontraba condensada en la estructura game_t.

En [Board.h](./Board.h) no hay cambios que merezcan mención.

## Estructuras:

Por su parte existen dos estructuras de creación propia que serán vitales para el código y juego, estas son:

board_t: Alojada en Board.h, en esta se aloja el tablero de juego, en definitiva, una matriz de _char_, además se guardan las dimensiones de dicha matriz.
```c
struct _board{
  size_t columna;
  size_t fila;
  char ** board;
};

typedef struct _board board_t;
```

_game_t_: se encuentra en Game.h veremos que se aloja un juego de la vida.
Los datos almacenados son la cantidad de _turnos_ que se realizaran, qué son los _ciclos_, la cantidad de los hilos que se crearán para repartirse el trabajo y una variable  _id_, que será necesaria en el momento de separar la tarea.

La variable __barrier_t__ es lo que se utiliza para sincronizar los distintos hilos.

```c
struct _game{
  board_t tableros[2];
  unsigned int ciclos, hilos,id;
  barrier_t * barrier;
};

typedef struct _game game_t;
```

## El tablero

En el caso de nuestra implementación por limitaciones al tratarse de un escenario generado en una computadora, que no dispone de recursos ilimitados, el tablero es finito, a diferencia del planteado por Conway. Además, el tablero está deformado para que si nos excedemos por alguno de los límites nos encontremos en el límite opuesto, tal como pasa en juegos como el [Pac-Man](https://es.wikipedia.org/wiki/Pac-Man).

El tablero es una matriz de _char_ esto para poder hacer de manera un poco más directa la lectura y escritura de los archivos de entrada y salida, los cuales se notaran en el punto posterior. La elección del tipo _char_ reduce cuatro veces el espacio que ocuparía la matriz si esta fuese del tipo _int_

## Archivos de entrada y salida

Para la entrada se utilizaran archivos **.game** los cuales traen como especificación:

* En la primera línea del archivo está dispuesto la cantidad de turnos del juego, las filas y las columnas.

* En el resto el tablero codificado en formato [RLE](https://es.wikipedia.org/wiki/Run-length_encoding)

Veamos el siguiente ejemplo:

```
7 5 5
5X
5X
1X3O1X
5X
5X
```

Que es equivalente al siguiente juego:
```
 Turnos: 7 Columnas: 5 Filas: 5
```
```
XXXXX
XXXXX
XOOOX
XXXXX
XXXXX
```

En el caso de la salida son archivos **.final** los cuales sólo contienen el tablero codificado en RLE y se verá de la sigiente forma:
```
5X
2X1O2X
2X1O2X
2X1O2X
5X
```
representando al siguiente tablero:
```
XXXXX
XXOXX
XXOXX
XXOXX
XXXXX
```


# Concurrencia
La parte concurrente de este trabajo es a la hora de realizar los distintos turnos. De la forma que se decidió realizar las actualizaciones no existen _zonas críticas_ de escritura y lectura. Esto pues se utilizan dos tableros, uno con el estado actual del juego que solo es para consulta y otro en el cual se van actualizando los nuevos valores, la no existencia de las zonas críticas se debe a que cada hilo solo modificara las filas que tenga asignadas, sin intervenir en las filas vecinas.

El problema que trae la concurrencia a este problema es el de sincronización entre hilos. Esto se debe a que no todos tendrán la misma carga de trabajo todo el tiempo. En esta implementación los hilos cooperan en una suerte de _divide y vencerás_ con el objetivo de reducir al mínimo el tiempo de ejecución. El trabajo será distribuido entre la cantidad de hilos. Esta cantidad dependerá directamente de la cantidad de unidades de cálculo que disponga el dispositivo en el cual nos encontremos, intentando siempre lanzar la cantidad más cercana de hilos posible a este número.

Un ejemplo: Supongamos que tenemos una máquina con 4 unidades de cómputo, por lo que se crearán 4 hilos (A, B, C, D) y supongamos que el tablero es de dimensión 8 filas y 9 columnas. Los hilos se repartirán el trabajo de la siguiente manera:
* Hilo A: filas 1,5
* Hilo B: filas 2,6
* Hilo C: filas 3,7
* Hilo D: filas 4,8


# Compilación y Ejecución:

Compilar el proyecto no debería dar muchos inconvenientes, esto gracias a que el mismo viene con un **makefile** en su interior.

Con solo pedir:

> make simulador

Se compilará todo el proyecto.

Para ejecutar basta con llamar desde consola a con el nobre del ejecutable __simulador__

Luego de esto se nos abrirá un prompt donde se ingresara el nombre del archivo de entrada con su extencion .game. La salida poseera el mismo nombre, solo que con la extensión *.final*.

Veamos un ejemplo con **ejemplo.game**
```BASH
user:~$ ./simulador
$>./simulador ejemplo.game
XXXXX
XXXXX
XOOOX
XXXXX
XXXXX
Ciclos: 7 Dim: 5 5
Salida generada en: ejemplo.final
user:~$
```


