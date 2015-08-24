-module(queen_album_resource).

-export([
    routes/0,
    init/1,
    to_html/2,
    to_json/2,
    content_types_provided/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(q_album_resource_state, {
    albums = []
    }).

routes() ->
    [
        {["albums"], ?MODULE, []}
    ].

-spec init(list()) -> {ok, term()}.
init([]) ->
    Albums = sqerl_rec:fetch_all(queen_album_obj),
    InitialState = #q_album_resource_state{
        albums=Albums
    },
    {{trace, "/tmp/traces"}, InitialState}.

content_types_provided(Req, State) ->
    {[
        {"application/json", to_json},
        {"text/html", to_html}
    ], Req, State}.

to_html(ReqData, State=#q_album_resource_state{albums=Albums}) ->
    HTML = "<html><body>"
    ++ "<h2>Queen Albums</h2>"
    ++ "<ul>"
    ++ [ io_lib:format("<li>~s (~p)</li>~n",
        [
            queen_album_obj:getval('name', Album),
            queen_album_obj:getval('year', Album)
        ])
    || Album <- Albums]
    ++ "</ul></body></html>",
    {HTML, ReqData, State}.

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State=#q_album_resource_state{albums=Albums}) ->
    AlbumsForJSON = [
        {[
            {<<"id">>,queen_album_obj:getval('id',Album)},
            {<<"name">>,queen_album_obj:getval('name',Album)},
            {<<"year">>,queen_album_obj:getval('year',Album)}
        ]}
    || Album <- Albums],
    AlbumsJSON = jiffy:encode(AlbumsForJSON),
    {AlbumsJSON, ReqData, State}.
