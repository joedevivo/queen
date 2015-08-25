-module(pg_test_buddy).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

start(Config) ->
    prereq("pg_ctl"),
    prereq("psql"),

    PgName = "queen_common_test",
    PgPort = 5432,
    Dir = ?config(priv_dir, Config),
    PgData = filename:join(Dir, "pg_data"),
    PgLog = filename:join(Dir, "pg.log"),

    Config1 = [
        {pg_data, PgData},
        {pg_log, PgLog},
        {pg_port, PgPort},
        {pg_dbname, PgName},
        {pg_user, os:getenv("USER")}
    ] ++ Config,

    Config2 = start_postgres_maybe(Config1),
    configure_sqerl(Config2),
    Config2.

start_postgres_maybe(Config) ->
    case creatabase(Config) of
        postgres_not_running ->
            prereq("initdb"),
            prereq("createdb"),
            PgData = ?config(pg_data, Config),
            PgLog = ?config(pg_log, Config),
            PortStr = integer_to_list(?config(pg_port, Config)),
            cmd(["initdb", "-D", PgData]),
            cmd(["pg_ctl", "-D", PgData, "-l", PgLog, "-o \"-p", PortStr ++ "\" start"]),
            %% Wait for database to start
            timer:sleep(1000),
            cmd(["createdb", ?config(pg_user, Config)]),
            creatabase(Config),
            [{pg_buddy_start, true}|Config];
        ok ->
            Config;
        _ ->
            Config
    end.

creatabase(Config) ->
    QuotedDBName = "\"" ++ ?config(pg_dbname, Config) ++ "\"",
    Output = string:tokens(cmd([
        "psql", "-c", "'create database", ?config(pg_dbname, Config), ";'", "-U", ?config(pg_user, Config)
    ]), [$\s, $\n]),
    case Output of
        % ➜ psql -c 'create database travis_ci_test;' -U joe
        % psql: could not connect to server: No such file or directory
        %     Is the server running locally and accepting
        %     connections on Unix domain socket "/tmp/.s.PGSQL.5432"?
        %[$p, $s, $q, $l, $:, $\s, $c, $o, $u, $l, $d, $\s, $n, $o, $t, $\s, $c, $o, $n, $n, $e, $c, $t, $\s to server]
        %"psql: could not connect to server: No such file or directory" ++ _ ->
        ["psql:", "could", "not", "connect", "to", "server:"| _] ->
            %% Database not up
            postgres_not_running;
        %➜ psql -c 'create database travis_ci_test;' -U joe
        %ERROR:  database "travis_ci_test" already exists
        ["ERROR:", "database", QuotedDBName, "already", "exists"] ->
            %% already exists
            ok;
        %➜ psql -c 'create database travis_ci_test;' -U joe
        %CREATE DATABASE
        ["CREATE", "DATABASE"] ->
            ok;
        Other ->
            ct:pal("Bad output for create database: ~p", [Other]),
            ?assertEqual({bad_create, true}, {bad_create, false})
    end,

    %% still running, create the schema
    cmd(["psql", "-d", ?config(pg_dbname, Config), "-f", filename:join(code:priv_dir(queen), "databass.sql"), "-U", ?config(pg_user, Config)]),
    ok.

stop(Config) ->
    application:stop(sqerl),
    application:stop(pooler),
    case ?config(pg_buddy_start, Config) of
        true ->
            %% This will only stop a db that we started in start_postgres_maybe/1
            %% otherwise the command will fail, but it's ok.
            cmd(["pg_ctl -D", ?config(pg_data, Config), "-m fast", "stop"]);
        _ -> meh
    end,
    ok.

cmd(List) ->
    Command = space_join(List),
    ct:pal("Running Command: ~s~n", [Command]),
    Output = os:cmd(Command),
    ct:pal("Output: ~s~n", [Output]),
    Output.

configure_sqerl(Config) ->
    SqerlConfig = [
        {db_host, "localhost"},
        {db_port, ?config(pg_port, Config)},
        {db_user, ?config(pg_user, Config)},
        %% ignored since we are connecting locally as owning user
        {db_pass, ""},
        {db_name, ?config(pg_dbname, Config)},
        {idle_check, 10000},
        {prepared_statements, {sqerl_rec, statements, [[{app, queen}]]}},
        {column_transforms, []}
       ],
    set_env(sqerl, SqerlConfig),
    PoolConfig = [
        {name, sqerl},
        {max_count, 3},
        {init_count, 1},
        {start_mfa, {sqerl_client, start_link, []}}],
    ok = application:set_env(pooler, pools, [PoolConfig]),
    ct:pal("Loaded apps: ~p", [application:loaded_applications()]),
    {ok, _} = application:ensure_all_started(queen),
    {ok, _} = application:ensure_all_started(sqerl),
    ok.


set_env(App, Config) ->
    [ok = application:set_env(App, Key, Value) || {Key, Value} <- Config ].

prereq(Cmd) ->
    case os:find_executable(Cmd) of
        false ->
            ct:pal("could not find executable: ~s~n", [Cmd]),
            ?assert(false);
        _ ->
            %%Found it!
            true
    end.

space_join(L) ->
    [ [Elt, " "] || Elt <- L ].