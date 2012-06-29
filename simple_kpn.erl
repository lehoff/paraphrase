%%------------------------------------------------------------------------------
%% Author: Torben Hoffman <torben.hoffman@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(simple_kpn).

-compile(export_all).

chan(Name, ToList) ->
    receive
        Val ->
            [ To ! {Name, Val}
              || To <- ToList]
    end,
    chan(Name, ToList).

new_chan(Name, ToList) when is_list(ToList) ->
    Pid = spawn( fun() -> chan(Name, ToList) end ),
    register(Name, Pid),
    Pid;
new_chan(Name, To) ->
    new_chan(Name, [To]).

receive_all(Chs) ->
    receive_all(Chs, []).

receive_all([], Vals) ->
    Vals;
receive_all(Chs, Vals) ->
    receive
        {Ch, Val} ->
            receive_all( lists:delete(Ch,Chs),
                         [Val | Vals] )
    end.

nat(Ins, Out, N) ->
    Out ! N,
    Vals = receive_all(Ins),
    io:format("nat got ~p~n", [Vals]),
    nat(Ins, Out, N+1).

apply_f(In, Out, Fun) ->
    receive
        {In, Val} ->
            Out ! Fun(Val)
    end,
    apply_f(In, Out, Fun).

double(In, Out) ->
    Pid = spawn ( fun() -> apply_f(In, Out, fun(X) -> X * 2 end) end ),
    register(double, Pid),
    Pid.

square(In, Out) ->
    Pid = spawn ( fun() -> apply_f(In, Out, fun(X) -> X * X end) end),
    register(square, Pid),
    Pid.

run() ->
    new_chan(nats, [double, square]),
    new_chan(double2nat, nat),
    new_chan(square2nat, nat),

    double(nats, double2nat),
    square(nats, square2nat),
    Pid = spawn( fun() -> nat([double2nat, square2nat], nats, 0) end),
    register(nat, Pid).
