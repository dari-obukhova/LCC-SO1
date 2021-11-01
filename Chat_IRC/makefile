make: Client Server
	@echo compilado!

Client : Client.c
	gcc Client.c -o Cli -pthread

Server : Server.c conc_list.c
	gcc Server.c conc_list.c -o Srv -pthread