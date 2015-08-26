-module(queen_album_resource).

-export([
    routes/0,
    init/1,
    allowed_methods/2,
    post_is_create/2,
    create_path/2,
    to_html/2,
    to_json/2,
    content_types_provided/2,
    content_types_accepted/2,
    malformed_request/2,
    resource_exists/2,
    from_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(q_album_resource_state, {
    albums = [],
    decoded_body = undefined
}).

routes() ->
    [
        {["albums"], ?MODULE, []}
    ].

-spec init(list()) -> {ok, term()}.
init([]) ->
    {{trace, "/tmp/traces"}, #q_album_resource_state{}}.

content_types_provided(Req, State) ->
    {[
        {"application/json", to_json},
        {"text/html", to_html}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {"application/json", from_json}
    ], Req, State}.


allowed_methods(Req, State) ->
    {['GET','POST'], Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

create_path(Req, State) ->
    Id = uuid:uuid_to_string(uuid:get_v4()),
    {"/albums/" ++ Id, Req, State}.

malformed_request(ReqData, State) ->
    %% This runs on every request, but we only want it to happen on POST
    case wrq:method(ReqData) of
        'POST' ->
            BinMaybeJSON = wrq:req_body(ReqData),
            {JSON, IsMalformed} = try jiffy:decode(BinMaybeJSON) of
                DefinitelyJSON ->
                    {DefinitelyJSON, false}
            catch
                _:_ ->
                    {undefined, true}
            end,
            {IsMalformed, ReqData, State#q_album_resource_state{decoded_body=JSON}};
        _ ->
            {false, ReqData, State}
        end.

resource_exists(Req, State) ->
    case wrq:method(Req) of
        'GET' ->
            Albums = sqerl_rec:fetch_all(queen_album_obj),
            NewState = State#q_album_resource_state{
                albums=Albums
            },
            {true, Req, NewState};
        _ ->
            {true, Req, State}
        end.

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

from_json(Req, State=#q_album_resource_state{decoded_body=JSON}) ->
    %% Create path made us a UUID, let's get it
    ["albums", Id] = string:tokens(wrq:disp_path(Req), "/"),

    %% Now massage it into our sqerl record
    NewAlbum = queen_album_obj:setvals(
        [
            {id, Id},
            {name, ej:get([<<"name">>], JSON)},
            {year, ej:get([<<"year">>], JSON)}
        ],
        queen_album_obj:'#new'()
        ),

    %% Insert It
    [InsertedAlbum] = sqerl_rec:insert(NewAlbum),

    %% Let's report back the inserted object, so we know the new UUID in the client
    ReturnJSON = ej:set([<<"id">>], JSON, queen_album_obj:getval(id, InsertedAlbum)),
    ResponseBody = erlang:iolist_to_binary(jiffy:encode(ReturnJSON)),
    NewReq = wrq:set_resp_body(ResponseBody, Req),
    {true, NewReq, State}.
