#include "conc_list.h"
#include <stdlib.h>
#include <string.h>

conc_list * conc_list_init(){
  conc_list * newList = malloc(sizeof(conc_list));
  newList->list = NULL;
  pthread_mutex_init(&(newList->lock), NULL);
  return newList;
}

int conc_list_add(conc_list * list, char * nick, int socket){
  if (!list)
    return EXIT_FAILURE;
  pthread_mutex_lock(&(list->lock));
  
  conc_list_node * newNodo = malloc(sizeof(conc_list_node));
  
  newNodo->nick = nick;
  newNodo->socket = socket;
  
  newNodo->siguiente = list->list;
  list->list = newNodo;

  pthread_mutex_unlock(&(list->lock));
  return EXIT_SUCCESS;
}

int conc_list_delete(conc_list * list){
  int salida = EXIT_FAILURE;
  pthread_mutex_lock(&(list->lock));
  if (list){
    if (list->list == NULL){
      free(list);
      salida = EXIT_SUCCESS;
    }
  }
  else
    salida = EXIT_FAILURE;
  pthread_mutex_unlock(&(list->lock));
  return salida;
}

int conc_list_rm(conc_list * list, void * dato, int type){
  if (!list){
    return EXIT_FAILURE;
  }
  int salida = EXIT_SUCCESS;
  pthread_mutex_lock(&(list->lock));
  conc_list_node * nodo = list->list;
  if (!nodo)
    salida = EXIT_FAILURE;
  else if (type){       //BUSQUEDA POR SOCKET
    int socket = *(int *)dato;
    int serch = EXIT_FAILURE;
     
    if (nodo->socket == socket){  //Nodo actual es el buscado
      list->list = nodo->siguiente;
      free(nodo);
      salida = EXIT_SUCCESS;
    }
    else{                         //El nodo pertenece al reto de la lista
      while (serch && nodo->siguiente){
        if (socket == nodo->siguiente->socket){
          conc_list_node * temp = nodo->siguiente;
          nodo->siguiente = temp->siguiente;
          free(temp);
          serch = EXIT_SUCCESS;
        }
        else
          nodo = nodo->siguiente;
      }

      salida = serch;
    }
  }
  else {                // BUSQUEDA POR NICK
    char * nick = (char *) dato;
    int serch = EXIT_FAILURE;
     
    if (!strcmp(nodo->nick, nick)){ //Nodo actual es el buscado
      list->list = nodo->siguiente;
      free(nodo);
      salida = EXIT_SUCCESS;
    }
    else {
      while (serch && nodo->siguiente){
        if (strcmp(nick, nodo->siguiente->nick)){
          conc_list_node * temp = nodo->siguiente;
          nodo->siguiente = temp->siguiente;
          free(temp);
          serch = EXIT_SUCCESS;
        }
        else
          nodo = nodo->siguiente;
      }

      salida = serch;
    }
  }
  pthread_mutex_unlock(&(list->lock));
  return salida;
}

conc_list_node * conc_list_serch(conc_list * list, char * nick){
  if (!list)
    return NULL;
  pthread_mutex_lock(&(list->lock));
  conc_list_node * nodo = list->list;
  char salida = 1;
  while (nodo && salida){
    if (strcmp(nick, nodo->nick))
      nodo = nodo->siguiente;
    else
      salida = 0;
  }
  pthread_mutex_unlock(&(list->lock));
  return nodo;
}

