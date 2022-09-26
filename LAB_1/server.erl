-module(server).
%% Exported Functions
-export([start/0]).

%% API Functions
start() ->
    ServerPid = spawn(fun() -> process_requests([]) end),   % Es crea un nou proces, la seva funció es process_requests.
    register(myserver, ServerPid).                          % Canviar el pid per un nom simple; "myserver"

process_requests(Clients) ->
    receive                             % Es suspen el proces esperant per un missatge
        {client_join_req, Name, From} ->
            NewClients = [From|Clients],  %% NewClients es la nova llista amb el nou client
            broadcast(NewClients, {join, Name}),
            process_requests(NewClients);  %% La nova llista es NewClients
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(Name, Clients),  %% Esborrem "Name" de la llista, i ho guardem a NewClients
            broadcast(Clients, {exit, Name}),  %% Enviem el missatge de exit a tothom.
            From ! exit,
            process_requests(NewClients);  %% La nova llista es NewClients.
        {send, Name, Text} ->
            broadcast(Clients, Text),  %% Envia el missatge Text al usuari Name.
            process_requests(Clients);
        disconnect ->
            unregister(myserver)
    end.

%% Local Functions 
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,      % Junta el nom del Peer amb el missatge en una funció
    lists:map(Fun, PeerList).                   % Reenvia la funció a cada Peer del PeerList.
