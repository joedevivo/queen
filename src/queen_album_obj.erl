-module(queen_album_obj).

-compile({parse_transform, sqerl_gobot}).

-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-record(?MODULE, {
        id :: binary(),
        name :: binary(),
        year :: integer()
    }).

'#insert_fields'() ->
    [name, year].

'#update_fields'() ->
    [name, year].

'#statements'() ->
    [default,
       {fetch_all, sqerl_rec:gen_fetch_all(?MODULE, year)}
    ].

'#table_name'() ->
    "albums".