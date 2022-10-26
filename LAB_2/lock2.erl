-module(lock2).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId);
        stop -> 
            ok
    end.

open(Nodes, MyId) ->
    receive
        {take, Master, Ref} ->
            Refs = requests(Nodes, MyId),
            wait(Nodes, Master, Refs, [], Ref, MyId);
        {request, From,  Ref} ->
            From ! {ok, Ref},
            open(Nodes, MyId);
        stop ->
            ok
    end.

requests(Nodes, MyId) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R}, 
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, MyId) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId) ->
    receive
        {request, From, Ref} ->
            wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId);
        {ok, Ref, id} ->
            if 
                MyId < id->
                    NewRefs = lists:delete(Ref, Refs),
                    wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId)
            end;
        release ->
            ok(Waiting, MyId),            
            open(Nodes, MyId)
    end.

ok(Waiting, MyId) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R, MyId} 
      end, 
      Waiting).

held(Nodes, Waiting, MyId) ->
    receive
        {request, From, Ref} ->
            held(Nodes, [{From, Ref}|Waiting], MyId);
        release ->
            ok(Waiting, MyId),
            open(Nodes, id)
    end.
