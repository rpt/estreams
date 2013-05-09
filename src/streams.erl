%%------------------------------------------------------------------------------
%% Copyright 2013 Krzysztof Rutka
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%------------------------------------------------------------------------------

%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%% @copyright 2013 Krzysztof Rutka
%% @doc Streams for Erlang.
-module(streams).

%% Create
-export([iterate/2,
         repeat/1,
         replicate/2,
         cycle/1]).

%% Use
-export([take/2,
         drop/2,
         nth/2,
         head/1, hd/1,
         tail/1, tl/1,
         is_finished/1]).

%% Manipulate
-export([map/2,
         intersperse/2]).

%% Common streams
-export([natural/0,
         random/1,
         fibonacci/0]).

%% Types
-export_type([stream/0]).

-type stream() :: fun(() -> {term(), stream()}) | finish.

%%------------------------------------------------------------------------------
%% Create
%%------------------------------------------------------------------------------

%% @doc Returns an infinite stream of repeated applications of F to X.
-spec iterate(F :: fun((term()) -> term()), X :: term()) -> S :: stream().
iterate(F, X) ->
    Iterate = fun(Acc, Iterate) ->
                      NewAcc = F(Acc),
                      {Acc, fun() -> Iterate(NewAcc, Iterate) end}
              end,
    fun() -> Iterate(X, Iterate) end.

%% @doc Returns an infinite stream of X values.
-spec repeat(X :: term()) -> Stream :: stream().
repeat(X) ->
    fun() -> {X, repeat(X)} end.

%% @doc Returns a stream of length N with X the value of every element.
-spec replicate(N :: integer(), X :: term()) -> S :: stream().
replicate(0, _) ->
    finish;
replicate(N, X) ->
    fun() -> {X, replicate(N - 1, X)} end.

%% @doc Returns the infinite repetition of the original list as a stream.
-spec cycle(L :: list()) -> S :: stream().
cycle([]) ->
    throw(empty_list);
cycle(L) ->
    Cycle = fun([], Cycle) ->
                    Cycle(L, Cycle);
               ([X | Xs], Cycle) ->
                    {X, fun() -> Cycle(Xs, Cycle) end}
            end,
    fun() -> Cycle(L, Cycle) end.

%%------------------------------------------------------------------------------
%% Use
%%------------------------------------------------------------------------------

%% @doc Takes first N elements from a stream.
-spec take(N :: integer(), S :: stream()) -> L :: [term()].
take(N, Stream) ->
    take(N, Stream, []).

take(0, _Stream, Acc) ->
    lists:reverse(Acc);
take(_, finish, Acc) ->
    lists:reverse(Acc);
take(N, Stream, Acc) ->
    {Value, NewStream} = Stream(),
    take(N - 1, NewStream, [Value | Acc]).

%% @doc Drops first N elements of a steam.
-spec drop(N :: integer(), S :: stream()) -> S2 :: stream().
drop(0, Stream) ->
    Stream;
drop(_, finish) ->
    finish;
drop(N, Stream) ->
    {_, NewStream} = Stream(),
    drop(N - 1, NewStream).

%% @doc Returns the Nth element from a stream.
-spec nth(N :: integer(), S :: stream()) -> X :: term().
nth(1, Stream) ->
    {Value, _} = Stream(),
    Value;
nth(_, finish) ->
    throw(stream_finished);
nth(N, Stream) ->
    {_, NewStream} = Stream(),
    nth(N - 1, NewStream).

%% @doc Returns the head of a stream, a.k.a. next element.
-spec head(S :: stream()) -> X :: term().
head(finish) ->
    throw(stream_finished);
head(Stream) ->
    {Value, _} = Stream(),
    Value.

%% @equiv head
-spec hd(S :: stream()) -> X :: term().
hd(Stream) ->
    head(Stream).

%% @doc Returns the tail of a stream.
-spec tail(S :: stream()) -> S2 :: stream().
tail(finish) ->
    throw(stream_finished);
tail(Stream) ->
    {_, NewStream} = Stream(),
    NewStream.

%% @equiv tail
-spec tl(S :: stream()) -> S2 :: stream().
tl(Stream) ->
    tail(Stream).

%% @doc Checks if stream is finished.
-spec is_finished(S :: stream()) -> boolean().
is_finished(finish) ->
    true;
is_finished(_Stream) ->
    false.

%%------------------------------------------------------------------------------
%% Manipulate
%%------------------------------------------------------------------------------

%% @doc Returns a stream obtained by applying F to each element of a stream S.
-spec map(F :: fun((term()) -> term()), S :: stream()) -> S2 :: stream().
map(_, finish) ->
    finish;
map(F, Stream) ->
    Map = fun(S, Map) ->
                  {Value, NewStream} = S(),
                  NewValue = F(Value),
                  case is_finished(NewStream) of
                      true ->
                          {NewValue, finish};
                      false ->
                          {NewValue, fun() -> Map(NewStream, Map) end}
                  end
              end,
    fun() -> Map(Stream, Map) end.

%% @doc Takes an element X and inserts it between the elements of a stream S.
-spec intersperse(X :: term(), S :: stream()) -> S2 :: stream().
intersperse(_, finish) ->
    finish;
intersperse(X, Stream) ->
    Inter = fun(S, Inter) ->
                    {Value, NewStream} = S(),
                    case is_finished(NewStream) of
                        true ->
                            {Value, finish};
                        false ->
                            NewerStream = fun() -> Inter(NewStream, Inter) end,
                            {Value, fun() -> {X, NewerStream} end}
                    end
            end,
    fun() -> Inter(Stream, Inter) end.

%% -spec foldl(F :: fun((term(), term()) -> term()),
%%             X :: term(), S :: stream()) -> S2 :: stream().
%% foldl(F, Init, Stream) ->

%% -spec foldr(F :: fun((term(), term()) -> term()),
%%             X :: term(), S :: stream()) -> S2 :: stream().
%% foldr(F, Init, Stream) ->

%% -spec concat(Ss :: [stream()]) -> S :: stream().
%% concat(Streams) ->

%%------------------------------------------------------------------------------
%% Common streams
%%------------------------------------------------------------------------------

%% @doc Returns a stream of natural numbers counting from 1 up.
-spec natural() -> S :: stream().
natural() ->
    iterate(fun(X) -> X + 1 end, 1).

%% @doc Returns a stream of random integers from 1 to N.
-spec random(N :: integer()) -> S :: stream().
random(N) ->
    iterate(fun(_) -> random:uniform(N) end, random:uniform(N)).

%% @doc Returns a stream of Fibonacci sequence numbers.
-spec fibonacci() -> S :: stream().
fibonacci() ->
    Fib = fun(Min2, Min1, Fib) ->
                  NewMin = Min2 + Min1,
                  {NewMin, fun() -> Fib(Min1, NewMin, Fib) end}
          end,
    fun() -> {0, fun() -> {1, fun() -> Fib(0, 1, Fib) end} end} end.
