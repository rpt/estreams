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
%% @doc QuickCheck tests for streams.
-module(estreams_eqc).

%% Properties
-export([prop_arithmetic_progression/0,
         prop_arithmetic_progression_sum/0,
         prop_geometric_series/0,
         prop_geometric_series_sum/0,
         prop_fibonacci_sequence/0,
         prop_take_stream_elements/0]).

-include_lib("eqc/include/eqc.hrl").

-define(N, 100).
-define(TESTS, 1000).

%%------------------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------------------

prop_arithmetic_progression() ->
    eqc:numtests(
      ?TESTS,
      ?FORALL(
         {A1, R, N}, {int(), int(), choose(1, 1000)},
         begin
             Stream = streams:iterate(fun(A) -> A + R end, A1),
             Nth = streams:nth(N, Stream),
             An = A1 + (N - 1) * R,
             ?WHENFAIL(
                io:format("~p /= ~p~n", [Nth, An]),
                Nth == An)
         end)).

prop_arithmetic_progression_sum() ->
    eqc:numtests(
      ?TESTS,
      ?FORALL(
         {A1, R, N}, {int(), int(), choose(1, 1000)},
         begin
             Stream = streams:iterate(fun(A) -> A + R end, A1),
             Sum = lists:sum(streams:take(N, Stream)),
             Sn = (2 * A1 + (N - 1) * R) / 2 * N,
             ?WHENFAIL(
                io:format("~p /= ~p~n", [Sum, Sn]),
                Sum == Sn)
         end)).

prop_geometric_series() ->
    eqc:numtests(
      ?TESTS,
      ?FORALL(
         {A1, Q, N}, {nat(), nat(), choose(1, 1000)},
         begin
             Stream = streams:iterate(fun(A) -> A * Q end, A1),
             Nth = streams:nth(N, Stream),
             An = int_pow(Q, N - 1) * A1,
             ?WHENFAIL(
                io:format("~p /= ~p~n", [Nth, An]),
                Nth == An)
         end)).

prop_geometric_series_sum() ->
    eqc:numtests(
      ?TESTS,
      ?FORALL(
         {A1, Q, N}, {nat(), choose(0, 1000), choose(1, 1000)},
         begin
             Stream = streams:iterate(fun(A) -> A * Q end, A1),
             Sum = lists:sum(streams:take(N, Stream)),
             Sn = case Q of
                      1 -> A1 * N;
                      _ -> A1 * (1 - int_pow(Q, N)) div (1 - Q)
                  end,
             ?WHENFAIL(
                io:format("~p /= ~p~n", [Sum, Sn]),
                Sum == Sn)
         end)).

prop_fibonacci_sequence() ->
    eqc:numtests(
      ?TESTS,
      ?FORALL(
         N, choose(0, 1000),
         begin
             Stream = streams:fibonacci(),
             Nth = streams:nth(N + 1, Stream),
             FibN = fibonacci(N),
             ?WHENFAIL(
                io:format("~p /= ~p~n", [Nth, FibN]),
                Nth == FibN)
         end)).

prop_take_stream_elements() ->
    eqc:numtests(
      ?TESTS,
      ?FORALL({Stream, Length}, stream(),
              case catch streams:take(?N, Stream) of
                  List when is_list(List) ->
                      case Length of
                          infinite ->
                              length(List) == ?N;
                          N when N =< ?N ->
                              length(List) == N;
                          _Else ->
                              length(List) == ?N
                      end;
                  _Else ->
                      false
              end)).

%%------------------------------------------------------------------------------
%% Generators
%%------------------------------------------------------------------------------

stream() ->
    oneof([simple_stream(),
           manipulated_stream()]).

simple_stream() ->
    oneof([finite_stream(),
           ?LET({F, X}, {function1(term()), term()},
                {streams:iterate(F, X), infinite}),
           ?LET(Term, term(),
                {streams:repeat(Term), infinite}),
           ?LET(List, non_empty(list(term())),
                {streams:cycle(List), infinite}),
           {streams:natural(), infinite},
           ?LET(N, choose(1, 1000),
                {streams:random(N), infinite}),
           {streams:fibonacci(), infinite}]).

finite_stream() ->
    oneof([?LET({Term, N}, {term(), choose(1, ?N)},
                {streams:replicate(N, Term), N})]).

manipulated_stream() ->
    oneof([?LET({S, N}, simple_stream(),
                {streams:map(fun(X) -> X end, S), N}),
           ?LET({S, N}, simple_stream(),
                {streams:intersperse(0, S),
                 case N of
                     infinite ->
                         infinite;
                     N ->
                         N * 2 - 1
                 end}),
           ?LET({{S1, N1}, {S2, N2}}, {finite_stream(), simple_stream()},
                {streams:concat(S1, S2),
                 case N2 of
                     infinite ->
                         infinite;
                     N2 ->
                         N1 + N2
                 end})]).

term() ->
    oneof([binary(),
           bitstring(),
           bool(),
           char(),
           int(),
           largeint(),
           nat(),
           real()]).

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

int_pow(X, Y) -> int_pow(X, Y, 1).

int_pow(_, 0, Z) -> Z;
int_pow(X, Y, Z) -> int_pow(X, Y - 1, Z * X).

fibonacci(N) ->
    {A, _} = fibonacci2(N),
    A.

fibonacci2(0) -> {0, 1};
fibonacci2(N) ->
    {A, B} = fibonacci2(N div 2),
    C = A * (2 * B - A),
    D = B * B + A * A,
    if
        N rem 2 == 0 ->
            {C, D};
        true ->
            {D, C + D}
    end.
