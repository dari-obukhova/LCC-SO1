# Ejercicio Semanal IPC

## ALUMNOS:
+ [Joaquin Caporalini](https://git.dcc.fceia.unr.edu.ar/jcaporalini)    e-mail: joaquicaporalini@gmail.com
+ [Daria Obukhova](https://git.dcc.fceia.unr.edu.ar/dobukhova)  e-mail: dari.obukhova@gmail.com

El ejercicio consta de la implementación de una sala de chat con la capacidad de
los usuarios puedan hablar entre ellos de forma instantánea. Estan implementados
un servidor y un cliente, tal que una vez establecida la conexión, los clientes
pueden conversar libremente. 

La diferencia con **Chat_IRC**:
El Servidor está implementado en Erlang. 

# Para compilar y ejecutar el Servidor:

$ erl
$ c(clientes).
$ c(comandos).
$ c(server). 
$ server:start(). 


# Para compilar y ejecutar el Cliente: 

$ gcc Client.c -o Cli -pthread
$ ./Cli 127.0.0.1 1234


Para cerrar el Cliente:  CTRL + C ; Y 

Para cerrar el Servidor: server:fin(#Port<0.XX>).







  
## Detalles de la Implementación

+ Longitud máxima del mensaje: 1024 caracteres.
+ Compilación con `make`: se esperan dos ejecutables `cliente` y `servidor`.
+ El cliente toma la dirección Ipv4 y puerto al momento de ejecutarlo de la
  siguiente forma:`$./cliente DirIp Puerto`.
+ El servidor toma el puerto al cual espera al cliente:`$./servidor Puerto`.
+ Asumimos que hay un solo salón donde se puede enviar mensajes de forma general
  a todo el mundo.
+ El cliente debe proveer un nombre (o *nickname*) que será único.
+ `/msg [nickname] [msg]` le enviará un el mensaje `msg` al usuario con nombre `nickname`.
+ `/exit` saldrá del cliente.
+ `/nickname [nickname]` intentará cambiar el nombre de usuario del cliente.


