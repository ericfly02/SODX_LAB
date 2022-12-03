-module(muty).
-behaviour(supervisor).
-export([start/7, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work, Node1, Node2, Node3, Node4) ->
    
    
    global:register_name(l1, spawn(Node1, Lock, start, [1]), fun global:notify_all_name/3),
    global:register_name(l2, spawn(Node2, Lock, start, [2]), fun global:notify_all_name/3),
    global:register_name(l3, spawn(Node3, Lock, start, [3]), fun global:notify_all_name/3),
    global:register_name(l4, spawn(Node4, Lock, start, [4]), fun global:notify_all_name/3),

    global_group:send(l1, {peers, [l2, l3, l4]}),
    global_group:send(l2, {peers, [l1, l3, l4]}),
    global_group:send(l3, {peers, [l1, l2, l4]}),
    global_group:send(l4, {peers, [l1, l2, l3]}),

    global:register_name(w1, spawn(Node1, worker, start, ["Eroc", l1, Sleep, Work])),
    global:register_name(w2, spawn(Node2, worker, start, ["x", l2, Sleep, Work])),
    global:register_name(w3, spawn(Node3, worker, start, ["Ixent", l3, Sleep, Work])),
    global:register_name(w4, spawn(Node4, worker, start, ["Roma", l4, Sleep, Work])),
    
    ok.

stop() ->
    w1 ! stop,
    w2 ! stop,
    w3 ! stop,
    w4 ! stop.