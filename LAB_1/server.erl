-module(server).
%% Exported Functions
-export([start/0]).

%% API Functions
start() ->
    ServerPid = spawn(fun() -> process_requests([]) end),   % Es crea un nou proces, la seva funció es process_requests.
    register(myserver, ServerPid).                          % Canviar el pid per un nom simple; "myserver"

process_requests(Clients) ->            % IMPORTANT: El que es guarda a la llista de clients son PIDs, no noms.
    receive                             % Es suspen el proces esperant per un missatge
        {client_join_req, Name, From} ->
            NewClients = [From|Clients],  %% NewClients es la nova llista amb el nou client
            broadcast(NewClients, {join, Name}),
            process_requests(NewClients);  %% La nova llista es NewClients
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(From, Clients),  %% Esborrem el pid de la llista, i ho guardem a NewClients
            broadcast(Clients, {leave, Name}),  %% Enviem el missatge de exit a tothom.
            From ! exit,
            process_requests(NewClients);  %% La nova llista es NewClients.
        {send, Name, Text} ->
            broadcast(Clients, {message, Name, Text}),  %% Envia el missatge Text de Name a Clients.
            process_requests(Clients);
        disconnect ->
            unregister(myserver)
    end.

%% Local Functions 
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,      % Envia a Peer el missatge Message
    lists:map(Fun, PeerList).                   % Reenvia la funció a cada Peer del PeerList.
