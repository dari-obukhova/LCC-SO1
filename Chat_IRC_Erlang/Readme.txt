Para compilar y ejecutar el Servidor:

$ erl
$ c(clientes).
$ c(comandos).
$ c(server). 
$ server:start(). 


Para compilar y ejecutar el Cliente: 

$ gcc Client.c -o Cli -pthread
$ ./Cli 127.0.0.1 1234


Para cerrar el Cliente:  CTRL + C ; Y 

Para cerrar el Servidor: server:fin(#Port<0.XX>).
