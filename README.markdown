This is a (somewhat) hacky attempt at making [meck](https://github.com/eproxus/meck) just a bit easier to use. My motivation to streamline and automate meck is based on two recent experiences: mocking out a complex database API to reduce testing dependencies and helping new Erlang programmers come up to speed with Eunit and meck.

_automeck_ has two main features. The first is a data driven format for generating simple mock functions which pattern match on their arguments to select the correct output. The file `priv/mocks.config` is an example of how to configure _automeck_ to generate a set of mocks.

The second feature allows _automeck_ to record function calls. _automeck_ records the module & function name, the call parameters, and the return value. The file `priv/record.config` is an example of how to configure _automeck_ to record function calls on a set of modules. After you've collected enough data you can tell _automeck_ to generate a set of mocks from the recorded data. The Erlang shell session listed below illustrates the API for this feature in more detail.

    1> {ok, Session} = automeck:record("priv/record.config").
    {ok,"/tmp/automeck_record.session"}
    %% Let's generate a bunch of function calls
    2> [{foo:test1(X), foo:test1(X, X + random:uniform(100)), bar:test(), bar:test(X, X)} || X <- lists:seq(1, 5)].
    [{2,-10,bar,2}|...]
    %% Let's look at the recorded data
    3> {ok, RawData} = file:read_file(Session).
    {ok, <<...>>}
    4> rp(RawData).
    <<"{foo, test1, [{[1,53], -52}]}.
       {bar, test, [{[], bar}]}.
       {bar, test, [{[1,1], 2}]}.
       {foo, test1, [{[2,56], -108}]}.
       {bar, test, [{[], bar}]}.
       {bar, test, [{[2,2], 4}]}.
       {foo, test1, [{[3,34], -93}]}...">>

----

    %% Finish the recording session and generate mock config file
    5> {ok, MockConfig} = automeck:finish_recording(Session).
    {ok,"/tmp/mocks.config"}
    6> MockedModules = automeck:mock(MockConfig).
    [bar, foo]
    %% Let's call a couple of mocked functions and an unmocked one
    7> foo:test1(1, 53).
    -52
    8> bar:test(2, 2).
    4
    %% The process tree crashes because this function wasn't mocked
    %% Same crash would occur if a mocked function was called with
    %% arguments not listed in mocks.config
    9> bar:test(1).
    =ERROR REPORT==== 27-Aug-2011::15:07:46 ===
    ** Generic server foo_meck terminating
    ** Last message in was {'EXIT',<0.31.0>,
                                   {undef,[{bar,test,[1]},
                                           {erl_eval,do_apply,5},
                                           {shell,exprs,7},
                                           {shell,eval_exprs,7},
                                           {shell,eval_loop,3}]}}


_automeck_ can also combine multiple recording sessions into a single mocks.config via `automeck:combine_recordings/2`. This should simplify the case where automeck is reconfigured multiple times during a test suite run, e.g. eunit's foreach and foreachx constructs. The merging process is smart enough to find conflicting function calls and abort the merge. A conflicting function call is one where the same function (same module name, function name and arity) is called in different sessions with the same inputs but returns differing results. There's no way for automeck to handle this cleanly so aborting the merge process is the only sane option.

_automeck_ started out as an experimental hack but I think these features are generally useful for testing and understanding non-trivial Erlang systems.

_automeck_'s lack of docs, specs, and tests will be addressed either as I have time or as I receive pull requests. Bug reports and/or suggestions also welcome :-)
