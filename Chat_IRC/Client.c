/* RemoteClient.c
   Se introducen las primitivas necesarias para establecer una conexión simple
   dentro del lenguaje C utilizando sockets.
*/
/* Cabeceras de Sockets */
#include <sys/types.h>
#include <sys/socket.h>
/* Cabecera de direcciones por red */
#include <netdb.h>
/**********/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <pthread.h>
#include <signal.h>

/*
  El archivo describe un sencillo cliente que se conecta al servidor establecido
  en el archivo RemoteServer.c. Se utiliza de la siguiente manera:
  $cliente IP port
 */

void error(char *msg){
  exit((perror(msg), 1));
}

int salida=1;
int sockete;

void * escuchar(void * args){
  char buff[1060]; //Nick + msg
  char old[1060];
  
  buff[0] = buff[1059] = '\0';
  old[0] = old[1059] = '\0';

  ssize_t safe = 0;

  for(;salida || strcmp("/exit\n", buff);){
    safe = recv(sockete, buff, sizeof(buff), MSG_DONTWAIT);
    buff[safe] = '\0';
    if (strcmp(old, buff))
      strcpy(old, buff);
    else
      buff[0] = '\0';
    
    printf("%s",buff);
  }
  printf("NO Escucho mas!!!\n");
  pthread_exit(EXIT_SUCCESS);
}

void * hablar(void * args){
  char buff[1024];
  buff[0] = buff[1023] = '\0';

  ssize_t safe = 0;
  
  char nick[32];
  nick[0] = nick[31] = '\0';
  
  safe = read(STDIN_FILENO, nick, sizeof(nick));
  nick[safe-1] = '\0';
  
  send(sockete, nick, strlen(nick)+1 ,0);

  for(;strcmp("/exit\n", buff);){
    fflush(STDIN_FILENO);
    safe = read(STDIN_FILENO, buff, sizeof(buff));
    if (buff[0] == '\n')
      buff[1] = '\0';
    if (salida == -1){
      if (buff[0] == 'y' || buff[0] == 'Y'){
        strcpy(buff,"/exit\n");
        safe = 7;
        buff[safe] = '\0';
        send(sockete, buff, strlen(buff)+1 ,0);
      }
      else{
        salida = 1;
        safe = 0;
      }
    }
    else{
      buff[safe] = '\0';
      send(sockete, buff, strlen(buff)+1 ,0);
    }
  }

  printf("No hablo mas\n");
  salida = 0;
  pthread_exit(EXIT_SUCCESS);
}

void handler(int arg){
  printf("Usted desea salir? y/n\n");
  salida = -1;
  return;
}


int main(int argc, char **argv){
  signal(SIGINT, handler);
  int sock;
  struct addrinfo *resultado;

  /*Chequeamos mínimamente que los argumentos fueron pasados*/
  if(argc != 3){
    fprintf(stderr,"El uso es \'%s IP port\'\n", argv[0]);
    exit(1);
  }

  /* Inicializamos el socket */
  if( (sock = socket(AF_INET , SOCK_STREAM, 0)) < 0 )
    error("No se pudo iniciar el socket");

  /* Buscamos la dirección del hostname:port */
  if (getaddrinfo(argv[1], argv[2], NULL, &resultado)){
    fprintf(stderr,"No se encontro el host: %s \n",argv[1]);
    exit(2);
  }

  if(connect(sock, (struct sockaddr *) resultado->ai_addr, resultado->ai_addrlen) != 0)
    /* if(connect(sock, (struct sockaddr *) &servidor, sizeof(servidor)) != 0) */
    error("No se pudo conectar :(. ");

  printf("La conexión fue un éxito! Insertar un nick:\n");
  /* ################################################ */
                  /* ZONA IMPORTANTE */
  /* ################################################ */
  sockete = sock;
  pthread_t ths[2];

  /* Crear NTHS hilos */
  
  if (pthread_create( &ths[0]
                    , NULL
                    , escuchar
                    , (void *)(NULL))
      != 0){
          perror("Falló la creación de un hilo");
          exit(EXIT_FAILURE);
      }
  if(pthread_create( &ths[1]
                      , NULL
                      , hablar
                      , (void *)(NULL))
        != 0){
            perror("Falló la creación de un hilo");
            exit(EXIT_FAILURE);
        }
	/* Esperamos a que todos los threads terminen */
	
  if(pthread_join(ths[0] , NULL) != 0){
    perror("Falló la espera de un hilo");
    exit(EXIT_FAILURE);
    printf("Terminó hilo %d\n", 0);
  }

  if(pthread_join(ths[1] , NULL) != 0){
    perror("Falló la espera de un hilo");
    exit(EXIT_FAILURE);
    printf("Terminó hilo %d\n", 1);
  }

        /* Terminamos */
        printf("Listo, muy rico todo\n");

  /* Cerramos :D!*/
  freeaddrinfo(resultado);
  close(sock);

  return 0;
}
