# Streams for Erlang [![Build Status][travis_ci_image]][travis_ci]

Inspired by Haskell's infinite lists. Tested with QuickCheck (mini).

## How to use them?

 * Run `make` to build.
 * Run `make test` to run the tests.
 * Add as a dependency to your `rebar.config`:

``` erlang
{deps, [{estreams, ".*", {git, "git://github.com/rpt/estreams.git"}}]}.
```

## API

### Create

 * **iterate/2** - Returns an infinite stream of repeated applications of F to
   X.

    ``` erlang
    iterate(F :: fun((term()) -> term()), X :: term()) -> S :: stream().
    ```

    ``` erlang
    x> streams:take(10, streams:iterate(fun(X) -> X + 1 end, 1)).
    [1,2,3,4,5,6,7,8,9,10]
    ```

 * **repeat/1** - Returns an infinite stream of X values.

    ``` erlang
    repeat(X :: term()) -> S :: stream()
    ```

    ``` erlang
    x> streams:take(10, streams:repeat(1)).
    [1,1,1,1,1,1,1,1,1,1]
    ```

 * **replicate/2** - Returns a stream of length N with X the value of every
   element.

    ``` erlang
    replicate(N :: integer(), X :: term()) -> S :: stream()
    ```

    ``` erlang
    x> streams:take(10, streams:replicate(5, 2)).
    [2,2,2,2,2]
    ```

 * **cycle/1** - Returns the infinite repetition of the original list as a
   stream.

    ``` erlang
    cycle(L :: list()) -> S :: stream().
    ```

    ``` erlang
    x> streams:take(10, streams:cycle([1, 2, 3]).
    [1,2,3,1,2,3,1,2,3,1]
    ```

### Use

 * **take/2** - Takes first N elements from a stream.

    ``` erlang
    take(N :: integer(), S :: stream()) -> L :: [term()].
    ```

    ``` erlang
    x> streams:take(10, streams:natural()).
    [1,2,3,4,5,6,7,8,9,10]
    ```

 * **drop/2** - Drops first N elements of a steam.

    ``` erlang
    drop(N :: integer(), S :: stream()) -> S2 :: stream().
    ```

    ``` erlang
    x> streams:take(10, streams:drop(10, streams:natural())).
    [11,12,13,14,15,16,17,18,19,20]
    ```

 * **nth/2** - Returns the Nth element from a stream.

    ``` erlang
    nth(N :: integer(), S :: stream()) -> X :: term().
    ```

    ``` erlang
    x> streams:nth(6, streams:natural()).
    6
    ```

 * **head/1** or **hd/1** - Returns the head of a stream, a.k.a. next element.

    ``` erlang
    head(S :: stream()) -> X :: term().
    hd(S :: stream()) -> X :: term().
    ```

 * **tail/1** or **tl/1** - Returns the tail of a stream.

    ``` erlang
    tail(S :: stream()) -> S2 :: stream().
    tl(S :: stream()) -> S2 :: stream().
    ```

 * **is_finished/1** - Checks if stream is finished.

    ``` erlang
    is_finished(S :: stream()) -> boolean().
    ```

    ``` erlang
    x> streams:is_finished(streams:replicate(3, 1)).
    false
    x> streams:is_finished(streams:drop(3, streams:replicate(3, 1))).
    true
    ```

### Manipulate

 * **map/2** - Returns a stream obtained by applying F to each element of a
   stream S.

    ``` erlang
    map(F :: fun((term()) -> term()), S :: stream()) -> S2 :: stream().
    ```

    ``` erlang
    x> streams:take(10, streams:map(fun(X) -> X * 2 end, streams:natural())).
    [2,4,6,8,10,12,14,16,18,20]
    ```

 * **intersperse/2** - Takes an element X and inserts it between the elements
   of a stream S.

    ``` erlang
    intersperse(X :: term(), S :: stream()) -> S2 :: stream().
    ```

    ``` erlang
    x> streams:take(10, streams:intersperse(0, streams:natural())).
    [1,0,2,0,3,0,4,0,5,0]
    ```

 * **concat/1** - Concatenates a list of streams into one stream.

    ``` erlang
    concat(Ss :: [stream()]) -> S :: stream().
    ```

    ``` erlang
    x> Streams = [streams:replicate(5, 0), streams:replicate(5, 1)].
    [#Fun<streams.x.xxxxxxxx>,#Fun<streams.x.xxxxxxxx>]
    x> streams:take(10, streams:concat(Streams)).
    [0,0,0,0,0,1,1,1,1,1]
    ```

 * **concat/2** - Concatenates two streams into one.

    ``` erlang
    concat(S1 :: stream(), S2 :: stream()) -> S :: stream().
    ```

    ``` erlang
    x> {Stream1, Stream2} = {streams:replicate(5, 0), streams:replicate(5, 1)}.
    {#Fun<streams.x.xxxxxxxx>,#Fun<streams.x.xxxxxxxx>}
    x> streams:take(10, streams:concat(Stream1, Stream2)).
    [0,0,0,0,0,1,1,1,1,1]
    ```

### Common streams

 * **natural/0** - Returns a stream of natural numbers counting from 1 up.

    ``` erlang
    natural() -> S :: stream().
    ```

    ``` erlang
    x> streams:take(10, streams:natural()).
    [1,2,3,4,5,6,7,8,9,10]
    ```

 * **random/1** - Returns a stream of random integers from 1 to N.

    ``` erlang
    random(N :: integer()) -> S :: stream().
    ```

    ``` erlang
    x> streams:take(10, streams:random(10)).
    [5,8,10,6,4,6,10,7,5,6]
    ```

 * **fibonacci/0** - Returns a stream of Fibonacci sequence numbers.

    ``` erlang
    fibonacci() -> S :: stream().
    ```

    ``` erlang
    x> streams:take(10, streams:fibonacci()).
    [0,1,1,2,3,5,8,13,21,34]
    ```

## TODO

 * More functions to create, use and manipulate streams
 * Parse transform for lists-like pattern matching

## Note

For the `eqcmini` tests to work this project uses a customized version of
`rebar` compiled with R13B04. The changes were merged into the official
`rebar/rebar` repository with rebar/rebar#75.

[travis_ci]: http://travis-ci.org/rpt/estreams
[travis_ci_image]: https://secure.travis-ci.org/rpt/estreams.png
