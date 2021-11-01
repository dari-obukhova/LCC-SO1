/* RemoteMultiThreadServer.c */
/* Cabeceras de Sockets */
#include <sys/types.h>
#include <sys/socket.h>
/* Cabecera de direcciones por red */
#include <netinet/in.h>
/**********/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
/**********/
/* Threads! */
#include <pthread.h>
#include "conc_list.h"

#include <unistd.h>

#include <signal.h> /* Signals */

/* Anunciamos el prototipo del hijo */
void *child(void *arg);
/* Definimos una pequeña función auxiliar de error */
void error(char *msg);

conc_list * clientList = NULL; //Lista de los clientes conectados

int mainSock;//Socket principal, donde se esperan a los clientes.

/* Asumimos que el primer argumento es el puerto por el cual escuchará nuestro
servidor */

/* Maxima cantidad de cliente que soportará nuestro servidor */
#define MAX_CLIENTS 25

/* Mensaje a todos los conectados*/
int msg_all(conc_list * list, char * nick, char * msg){
  if (!list)
    return 0;
  pthread_mutex_lock(&(list->lock));

  char emisor[1060]; emisor[0] = '['; emisor[1] = '\0';
  strncat(emisor,nick,33);
  strncat(emisor,"] ",3);
  strncat(emisor, msg, 1024);

  conc_list_node * nodo = list->list;
  while (nodo){
    send(nodo->socket, emisor, strlen(emisor) + 1, 0);
    nodo = nodo->siguiente;
  }
  pthread_mutex_unlock(&(list->lock));
  return 1;
}

/* Mensaje por privado */
int msg_nick(conc_list * list, char * nickFrom, char * nickTo, char * msg){
  if (!list)
    return 0;
  pthread_mutex_lock(&(list->lock));

  char emisor[1060]; emisor[0] = '['; emisor[1] = '\0';
  strncat(emisor,nickFrom,33);
  strncat(emisor,"] ",3);
  strncat(emisor, msg, 1024);

  printf("MSG priv SEND\n");
  conc_list_node * nodo = list->list;
  char salida = 1;
  while (nodo && salida){
    if (strcmp(nickTo, nodo->nick))
      nodo = nodo->siguiente;
    else{
      send(nodo->socket, emisor, strlen(emisor) + 1, 0);
      salida = 0;
    }
  }
  pthread_mutex_unlock(&(list->lock));
  return !salida;
}

/* Manejo de la señal para terminar la ejecucion (^C) */
void handler(int arg){
  if (conc_list_delete(clientList)){
    printf("\nTodavia existen usuarios conecctados.\n");
    msg_all(clientList, "SRV", "El servidor debe de cerrar, por favor desconectarce.\n");
  } else {
    clientList = NULL;
    close(mainSock);
    exit(0);
  }

  return;
}

int main(int argc, char **argv){
  int *soclient;
  struct sockaddr_in servidor, clientedir;
  socklen_t clientelen;
  pthread_t thread;
  pthread_attr_t attr;

  signal(SIGINT, handler);

  clientList = conc_list_init();

  if (argc <= 1) error("Faltan argumentos");

  /* Creamos el socket */
  if( (mainSock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    error("Socket Init");

  /* Creamos a la dirección del servidor.*/
  servidor.sin_family = AF_INET; /* Internet */
  servidor.sin_addr.s_addr = INADDR_ANY; /**/
  servidor.sin_port = htons(atoi(argv[1]));

  /* Inicializamos el socket */
  if (bind(mainSock, (struct sockaddr *) &servidor, sizeof(servidor)))
    error("Error en el bind");

  printf("Binding successful, and listening on %s\n",argv[1]);

  /************************************************************/
  /* Creamos los atributos para los hilos.*/
  pthread_attr_init(&attr);
  /* Hilos que no van a ser *joinables* */
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
  /************************************************************/

  /* Ya podemos aceptar conexiones */
  if(listen(mainSock, MAX_CLIENTS) == -1)
    error(" Listen error ");

  for(;;){ /* Comenzamos con el bucle infinito*/
    /* Pedimos memoria para el socket */
    soclient = malloc(sizeof(int));

    /* Now we can accept connections as they come*/
    clientelen = sizeof(clientedir);
    if ((*soclient = accept(mainSock
                          , (struct sockaddr *) &clientedir
                          , &clientelen)) == -1)
      error("No se puedo aceptar la conexión. ");

    /* Le enviamos el socket al hijo*/
    pthread_create(&thread , NULL , child, (void *) soclient);

    /* El servidor puede hacer alguna tarea más o simplemente volver a esperar*/
  }

  /* Código muerto */
  close(mainSock);

  return 0;
}

enum comandos{
  msgAll,
  msgPriv,
  cambiNick,
  salir
};

int comando(char * buff){
  if (!strncmp("/msg ", buff, 5))
    return msgPriv;
  if (!strncmp("/nick ", buff, 6))
    return cambiNick;
  if (!strncmp("/exit", buff, 5))
    return salir;
  return msgAll;
}

int remplazo_espacio(char * buf){
  int i = 0;
  for (;buf[i] != ' ' &&  buf[i] != '\0';i++);
  if (buf[i] == ' '){
    buf[i] = '\0';
    return i+1;
  }
  if (buf[i] == '\0')
    return -1;
}

void * child(void *_arg){
  int socket = *(int*) _arg;
  char buf[1024];
  buf[0] = '\0';
  ssize_t safe = 0;

  //Nick inicial:
  char nick[32]; nick[31] = nick[0] = '\0';
  safe = recv(socket, nick, sizeof(nick), 0);
  nick[safe] = '\0';

  //Confirmacion interna de que hay coneccion.
  conc_list_add(clientList, nick, socket);     //Pasa a ser un cliente listado.
  printf("Hilo[%ld] --> CONECT\n", pthread_self());

  //Sala de chat
  for (;strcmp("/exit\n", buf);){
    //Resepsion del mensaje
    safe = recv(socket, buf, sizeof(buf), 0);
    buf[safe] = '\0';
    switch (comando(buf)) {
      case (msgPriv):
        safe = remplazo_espacio(&(buf[5]));
        if (safe != -1){
          safe = msg_nick(clientList, nick, &(buf[5]), &(buf[safe+5])); //VER!
          if (!safe)
            send(socket, "[SRV] Destinatario no encontrado\n\0", 35, 0);
        }
        else
          send(socket, "[SRV] Comando de mensaje privado no valido: Revise [NICK] o mensaje\n\0", 70, 0);
        break;
      case (cambiNick):
        buf[safe-2] = '\0';
        strncpy(nick, &(buf[6]),32); 
        send(socket, "[SRV] Nick actualizado\n\0", 24, 0);
        break;
      case (salir):
        send(socket, buf, strlen(buf) + 1, 0);
        break;
      default:
        msg_all(clientList, nick, buf);
        break;
    }
    //Identificacion del objetivo
    printf("SRV~ [%s] %s", nick, buf);
    //send(socket, buf, strlen(buf) + 1, 0);
  }
  conc_list_rm(clientList, &socket, 1);
  printf("Hilo[%ld] --> Recv: %s\n", pthread_self(), buf);

  free((int*)_arg);
  return NULL;
}

void error(char *msg){
  exit((perror(msg), 1));
}
