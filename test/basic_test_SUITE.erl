-module(basic_test_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() -> [html, json, post_live_at_wembley].

init_per_testcase(_, Config) ->
    ibrowse:start(),
    pg_test_buddy:start(Config).

end_per_testcase(_, Config) ->
    pg_test_buddy:stop(Config),
    ok.

html(_Config) ->
    {ok, "200", ResponseHeaders, ResponseBody } = ibrowse:send_req(
         "http://localhost:8080/albums", [
        {"Accept", "text/html"}
        ], get),
    ct:pal("Response: ~p", [ResponseBody]),
    [$<,$h,$t,$m,$l|_T] = ResponseBody,
    ok.

json(_Config) ->
    {ok, "200", ResponseHeaders, ResponseBody } = ibrowse:send_req(
         "http://localhost:8080/albums", [
        {"Accept", "application/json"}
        ], get),
    ct:pal("Response: ~p", [ResponseBody]),
    JSON = jiffy:decode(ResponseBody),
    15 = length(JSON),
    ok.

post_live_at_wembley(_Config) ->
    {ok, "201", ResponseHeaders, ResponseBody } = ibrowse:send_req(
         "http://localhost:8080/albums", [
        {"Content-Type", "application/json"},
        {"Accept", "application/json"}
        ], post, "{\"name\":\"Live at Wembley '86\",\"year\":1992}"),
    ct:pal("Response: ~p", [ResponseBody]),
    JSON = jiffy:decode(ResponseBody),

    {ok, "200", ResponseHeaders2, ResponseBody2 } = ibrowse:send_req(
         "http://localhost:8080/albums", [
        {"Accept", "application/json"}
        ], get),
    ct:pal("Response2: ~p", [ResponseBody2]),
    JSON2 = jiffy:decode(ResponseBody2),
    16 = length(JSON2),
    ok.