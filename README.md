# Bootstrapping an Erlang REST Application

Welcome to bootstrapping an Erlang REST Application, let's get started.

## Rebar3

Rebar has been a very important part of Erlang for years now. In fact,
I never knew an Erlang without it, which is good. Imagine a Java
without Ant or Maven. Compiling files with `erlc` and thinking a
complicated `Makefile` was the "solution". So, in short, rebar is a
build tool for Erlang.

If rebar was *a* build tool for Erlang, then rebar3 is *the* build
tool for Erlang. Rebar3 is almost done, but the good news is that's
it's ready to use!  You can download the latest nightly
[here](https://s3.amazonaws.com/rebar3/rebar3). Go ahead and copy it
somewhere on your path (I use `/usr/local/bin`) and don't forget a
`chmod +x`.

Rebar3 does a lot of things right, including a feature that's been
missing from rebar since the beginning. Quality documentation.

Now that you have rebar3, we're going to work on building an
application, and we can talk more about rebar3 specifics as we go.


## Webmachine

Webmachine is an application framework for building REST applications
with HTTP semantic awareness. It's built on top of Mochiweb, an Erlang
web server.

To get started with Webmachine and rebar3 is pretty easy. We'll start
by making sure rebar3 has access to the Webmachine template. Some
super cool community members contributed to this :D. We'll only have
to do this once.

Go ahead and check out
`https://github.com/webmachine/webmachine-rebar3-template.git` to
`~/.config/rebar3/templates`.

Now any time you want to create a new Webmachine application, it's as
easy as `rebar3 new webmachine $your_app_name`

Let's try it! Make sure you're in the directory you want your
application to live. I mean you could move it, so I guess it doesn't
matter.

`rebar3 new webmachine queen`

Why `queen`? We're going to build a web service for data about Queen
albums because I was listing to Queen while writing this, and terrible
at coming up with examples. However, I am great at puns, so on with
WebMaQueen!

### The Webmachine Template

Let's look what it did:

```
➜ rebar3 new webmachine queen
===> Writing queen/README.md
===> Writing queen/Makefile
===> Writing queen/rebar.config
===> Writing queen/.gitignore
===> Writing queen/src/queen.app.src
===> Writing queen/src/queen_app.erl
===> Writing queen/src/queen_sup.erl
===> Writing queen/src/queen_config.erl
===> Writing queen/src/queen_resource.erl
===> Writing queen/config/sys.config
===> Writing queen/config/vm.args
```

#### README.md

The README.md will cover some stuff, but you'll want to modify it as
your application becomes more complex. Right now it's not
persistent. As we add things like database, we're going to probably
want to update the readme explaining how it works.

#### Makefile

The `Makefile` seems redundant. It pretty much pulls down `rebar3` if
you don't have it. You have it, so it's silly. Philosophically I think
every open source project should have a Makefile that either builds
your project with `make all` or explains why you can't. So this
makefile isn't for you. It's for people who build your project later.

#### .gitignore

The `.gitignore` ignores rebar3 artifacts.


#### rebar.config

The `rebar.config` is the first Erlangy thing we're going to see in
there. It's a config file, written in erlang. I have feelings about
that:
[Cuttlefish](https://www.youtube.com/watch?v=rJmYoPvppRE). However,
those feels are about not needing to know Erlang to deploy Erlang
applications. I think that's valid. Since you probably need to know
Erlang to write Erlang, configuring your build in Erlang is ok by me.

Each configuration section is broken up by a period. Erlang is kind of
punctuated like English, so a period ends a statement. The first
statement is about dependencies we're pulling in.

```erlang
%%-*- mode: erlang -*-
{deps, [
    {webmachine, "1.10.*",
        {git, "git://github.com/webmachine/webmachine", {branch, "develop"}}}
]}.
```

`deps` is about what dependencies we need to include in our app, and
this syntax works the same for rebar2 and rebar3. What even is that syntax?

It's saying "pull webmachine from that url, on branch 'develop'". It
will pull down the source and compile it as part of your build

Rebar3 makes this easier by using the [hex.pm](https://hex.pm) package
manager. Webmachine isn't on hex tho. I should fix that.

If it were on hex, we could just say

```erlang
{deps, [webmachine]}.
%% or
{deps, [{webmachine, "1.10.8"}]).
```

Now let's move to the next tuple, `relx`

```erlang
{relx, [{release, {'queen', "0.1.0"},
         ['queen',
          sasl]},

        {sys_config, "./config/sys.config"},
        {vm_args, "./config/vm.args"},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true}]
}.
```

This is structuring our release with `relx`. All the places you see
`'queen'` have been subbed in by our template. What's the rest doing?

```erlang
        {sys_config, "./config/sys.config"},
        {vm_args, "./config/vm.args"},
```

These are the two config files you'll find in any Erlang
application. `sys.config` is where you configure the specifics of your
application and applications you depend on. `vm.args` is specific to
starting the Erlang VM and we honestly won't need to worry about it
here.

`sys.config` is set to start our web app locally on port
`8080`. Pretty standard for a development web app.

Speaking of development, did you see `{dev_mode, true}` up there?
We're set up to symlink all our code into rebar3's build system, so we
can get on the fly changes as we edit. More on that later.

In rebar 2, this would have to be in it's own file called
`relx.config` and you'd need the `relx` executable. Rebar3 includes it
all.

Next up are `profiles` which are exclusive to rebar3.
```erlang

{profiles, [
    {dev, [
        {deps, [
            {sync, ".*", {git, "git://github.com/rustyio/sync.git", {branch, "master"}}}
        ]}
    ]},
    {prod, [
        {relx, [
            {dev_mode, false},
            {include_erts, true}
        ]}
    ]}
]}.
```

All we're saying here is when we're using the `dev` profile, add this
dependency called `sync`. This will help us with our dev workflow, but
doesn't belong in production.

Speaking of production, when we build this for `prod`, we'll want to
turn `dev_mode` off AND include an Erlang runtime so this can be a
self contained release.

In order to use profiles, just run `rebar3 as $profile`.

Let's try it!

```
➜ queen rebar3 as dev compile
===> Verifying dependencies...
===> Fetching sync ({git,"git://github.com/rustyio/sync.git",
                                {branch,"master"}})
===> Fetching webmachine ({git,
                                  "git://github.com/webmachine/webmachine",
                                  {ref,
                                   "6455b9d8685d9006f45f73c15386f5e9686a8c56"}})
===> Linking _build/default/lib/webmachine to _build/dev/lib/webmachine
===> Fetching mochiweb ({pkg,<<"mochiweb">>,<<"2.12.2">>})
===> Linking _build/default/lib/mochiweb to _build/dev/lib/mochiweb
===> Compiling mochiweb
===> Compiling webmachine
===> Compiling sync
===> Compiling queen
```

See? `as dev` pulled down both webmachine and sync!

You know what else cool happened? rebar3 generated a `rebar.lock` file.

```erlang
[{<<"mochiweb">>,{pkg,<<"mochiweb">>,<<"2.12.2">>},0},
 {<<"webmachine">>,
  {git,"git://github.com/webmachine/webmachine",
       {ref,"6455b9d8685d9006f45f73c15386f5e9686a8c56"}},
  0}].
```

What's this doing for us? It's locking the dependencies at the version
we just pulled in. If we want to change these versions from here on
out we have to use the commands `rebar3 unlock` and `rebar3
upgrade`. Hooray for stability. If you're thinking this is no big
deal, you're new to Erlang :P.

#### queen.app.src

```erlang
{application, queen,
 [
  {description, "queen"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  inets,
                  crypto,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { queen_app, []}},
  {env, [
      {web_ip, "0.0.0.0"},
      {web_port, 8080}
  ]}
 ]}.
 ```

This is pretty boilerplate. Notice the default values for the settings
in `sys.config` While this is not mandatory, I highly recommend
it. Also, we may need to play with the `applications` list as we bring
in more dependencies.

#### queen_app.erl

```erlang
-module(queen_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    queen_sup:start_link().

stop(_State) ->
ok.
```

This is our application behavior. OTP knows what to do with this, but
you can include any customized logic you need performed on startup or
shutdown.

#### queen_sup.erl

```erlang
-module(queen_sup).
-behaviour(supervisor).

%% External exports
-export([
  start_link/0
]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [queen_config:web_config()]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.
```

This is our OTP supervisor. You may have noticed
`queen_app:start_link()` calling `queen_sup:start_link()`

That thing we're binding to `Web` is a child spec. It tells the
supervisor things about how to manage the child process which in this
case is our webmachine_mochiweb module. You actually don't need to
know anything about that one.

#### queen_config.erl

`queen_config` is the module in which all of your configuration
lives. If you noticed, the function `web_config/0` is called by the
child process as it's default configuration

```erlang
web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].
```

This is all taken care of by the `.app.src` file and the `sys.config`
except that `dispatch` tuple which is set up in the `dispatch/0`
function.

```erlang
dispatch() ->
    lists:flatten([
        {[], queen_resource, []}
    ]).
```

This is a Webmachine route. It's pointing our default route of "/" to
our `queen_resource` module. We'll see how this syntax works as we add
stuff later.

#### queen_resource.erl

This is the actual code for our "/" endpoint.

Webmachine resources are all about callbacks, but if you don't define
them all, there are default passthrough functions that webmachine
uses. Here we only define `init/1` and `to_html/2`

`to_html/2` takes in our Request Data and our resource's internal
state. We don't have a state here and are just serving up static
content.

### Kicking the tires

So, let's see this bad boy in action

Use `rebar3 as dev shell` to start the app and get an Erlang REPL.

```
➜ queen rebar3 as dev shell
===> Verifying dependencies...
===> Compiling queen
Erlang/OTP 17 [erts-6.4.1] [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V6.4.1  (abort with ^G)
1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/v3.0/docs/releases)
===> Booted xmerl
===> Booted compiler
===> Booted syntax_tools
===> Booted mochiweb
===> Booted webmachine
===> Booted queen
===> Booted sasl

=PROGRESS REPORT==== 24-Aug-2015::09:34:10 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.146.0>},
                       {name,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 24-Aug-2015::09:34:10 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.147.0>},
                       {name,overload},
                       {mfargs,{overload,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 24-Aug-2015::09:34:10 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.145.0>},
                       {name,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 24-Aug-2015::09:34:10 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.148.0>},
                       {name,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 24-Aug-2015::09:34:10 ===
         application: sasl
          started_at: nonode@nohost

1>

```

Now now you can go to `http://localhost:8080/` and see that HTML output.

It's a kind of magic!

Now let's turn on `sync` by typing `sync:start().` into the console.

```erlang
1> sync:start().
Starting Sync (Automatic Code Compiler / Reloader)

=PROGRESS REPORT==== 24-Aug-2015::09:38:10 ===
          supervisor: {local,sync}
             started: [{pid,<0.2066.0>},
                       {name,sync_options},
                       {mfargs,{sync_options,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]
Scanning source files...

=PROGRESS REPORT==== 24-Aug-2015::09:38:10 ===
          supervisor: {local,sync}
             started: [{pid,<0.2067.0>},
                       {name,sync_scanner},
                       {mfargs,{sync_scanner,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 24-Aug-2015::09:38:10 ===
         application: sync
          started_at: nonode@nohost

=PROGRESS REPORT==== 24-Aug-2015::09:38:10 ===
          supervisor: {local,kernel_safe_sup}
             started: [{pid,<0.2070.0>},
                       {name,timer_server},
                       {mfargs,{timer,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,1000},
                       {child_type,worker}]
ok
2>
```

Now if you change the HTML text in `queen_resource`, you'll see this:

```
=INFO REPORT==== 24-Aug-2015::09:39:04 ===
/Users/joe/dev/joedevivo/queen/_build/dev/lib/queen/src/queen_resource.erl:0: Recompiled.

=INFO REPORT==== 24-Aug-2015::09:39:04 ===
queen_resource: Reloaded! (Beam changed.)
```

Reload the browser and you'll see the change!

Congrats! You're running a skeleton Webmachine application. Let's make it do more.

## Adding a dependency

Our webservice is going to be less about HTML and more about JSON, so
we should bring some kind of JSON library into our application. We're
going to use `jiffy` for parsing erlang terms to JSON and `ej` for an
easier development experience.

Let's add them both to `rebar.config` now:

```erlang
{deps, [
  {webmachine, "1.10.*",
     {git, "git://github.com/webmachine/webmachine", {branch, "develop"}}},
  {ej, ".*",
     {git, "git://github.com/seth/ej.git", {branch, "master"}}},
  {jiffy, ".*",
     {git, "https://github.com/davisp/jiffy.git", {tag, "0.14.2"}}}
]}.
```

There's a problem tho. Jiffy's not ready for rebar3. It works well
enough with most projects, but jiffy uses something called a NIF,
which is a bit of C compiled to be an external function call. Jiffy
uses a C based JSON encoder. Rebar2 used to compile this C for us, but
rebar3 doesn't. It's optional tho, we just need to add the option.

```erlang
{overrides, [
    {override, jiffy, [
        {plugins, [pc]},
        {provider_hooks, [
            {post, [
                {compile, {pc, compile}},
                {clean, {pc, clean}}
            ]}
        ]}
    ]}
]}.
```

you might have to run `rebar3 update` at this point to make sure you
have the port_compiler plugin locally.


Unfortunately, we'll have to exit the shell to do that. Also, `sync`
is smart, but it can't pull in new dependencies. Good news is that a
quick `q().` and if you go back in with `rebar3 as dev shell` you'll
see it pull in the new deps.

```
q().
rebar3 update
rebar3 as dev shell
```

And we're back!

What's cool is that now we can play with `jiffy` and `ej` in the shell
and see how they work

```
1> jiffy:encode({[]}).
<<"{}">>
2> OperaJSON = jiffy:encode({[{<<"name">>,<<"A Night at the Opera">>}]}).
<<"{\"name\":\"A Night at the Opera\"}">>
3> Opera = jiffy:decode(OperaJSON).
{[{<<"name">>,<<"A Night at the Opera">>}]}
4> ej:get({"name"}, Opera).
<<"A Night at the Opera">>
```

So now we have a little JSON library to use. Let's use it!

## Adding a resource

Let's add a "/albums" endpoint to our application.

We'll start with defining the route, in our resource's `routes/0` function.

```erlang
-module(queen_album_resource).

-export([
    routes/0
]).

-include_lib("webmachine/include/webmachine.hrl").

routes() ->
    [
        {["albums"], ?MODULE, []}
    ].
```

Now we have to go back and add a call to this to our
`queen_config:dispatch/0` function:

```erlang
-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
        {[], queen_resource, []},
        queen_album_resource:routes()
    ]).
```

We could have put the route here directly, but I like being able to
modify the route in the resource module.

Let's go back to it now and implement some callbacks.

We can use the same `init/1` function for now, but since we're going
to want to return JSON, we'll make a `to_json` function instead of
`to_html`.

```erlang
-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->

    Albums = [
        {[
            {<<"name">>, <<"Queen">>},
            {<<"year">>, 1973}
        ]},
        {[
            {<<"name">>, <<"Queen II">>},
            {<<"year">>, 1974}
        ]},
        {[
            {<<"name">>, <<"Sheer Heart Attack">>},
            {<<"year">>, 1974}
        ]},
        {[
            {<<"name">>, <<"A Night at the Opera">>},
            {<<"year">>, 1975}
        ]},
        {[
            {<<"name">>, <<"A Day at the Races">>},
            {<<"year">>, 1976}
        ]},
        {[
            {<<"name">>, <<"News of the World">>},
            {<<"year">>, 1977}
        ]},
        {[
            {<<"name">>, <<"Jazz">>},
            {<<"year">>, 1978}
        ]},
        {[
            {<<"name">>, <<"The Game">>},
            {<<"year">>, 1980}
        ]},
        {[
            {<<"name">>, <<"Flash Gordon">>},
            {<<"year">>, 1980}
        ]},
        {[
            {<<"name">>, <<"Hot Space">>},
            {<<"year">>, 1982}
        ]},
        {[
            {<<"name">>, <<"The Works">>},
            {<<"year">>, 1984}
        ]},
        {[
            {<<"name">>, <<"A Kind of Magic">>},
            {<<"year">>, 1986}
        ]},
        {[
            {<<"name">>, <<"The Miracle">>},
            {<<"year">>, 1989}
        ]},
        {[
            {<<"name">>, <<"Innuendo">>},
            {<<"year">>, 1991}
        ]},
        {[
            {<<"name">>, <<"Made in Heaven">>},
            {<<"year">>, 1995}
        ]}
    ],
    AlbumsJSON = jiffy:encode(Albums)
    {AlbumsJSON, ReqData, State}.
```

This is going to give us a 404. Why? The process has already started
and is not reading in new routes.

You can either leave the shell and come back in to get it to reload
the routes, but starting and stopping the queen application using the
`application` module should work too:

```erlang
application:stop(queen).
application:start(queen).
```

now browse to `http://localhost:8080/albums` and everything explodes!

It's because the default callbacks all expect `to_html`. Let's teach
it about our `to_json` callback, but first let's introduce the
webmachine debugger.

To include the webmachine debugger, add the following route to
`queen_config:dispatch()`.

```
{["wmtrace", '*'], wmtrace_resource, [{trace_dir, "/tmp/traces"}]}
```

Then remember our `queen_album_resource:init/1` function. We need to
change the return from `ok` to `{trace, "/tmp/traces"}`.

We also need to make sure it exists, so go ahead and `mkdir -p
/tmp/traces`.

So we do all that and restart our shell and we get the same error. big
whoop. Now browse to `http://localhost:8080/wmtrace` and hold on to
your hats! This is One Vision of how your HTTP request was routed.

A lot of these decision points in the graph have hooks into callbacks
for you. Pretty cool!

However, ours was a dead giveaway from the beginning.

```erlang
{error, {error,undef, [{queen_album_resource,to_html,...
```

`queen_album_resource:to_html` is undefined. Let's teach it about our
callback.

Here's a new callback, be sure to add `content_types_provided/2` to our
module's exports.

```erlang
content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.
```

Since we only have one content type, it uses it automatically in the
browser. But we could add to_html call back too,


```erlang
content_types_provided(Req, State) ->
    {[
       {"application/json", to_json},
       {"text/html", to_html}
    ], Req, State}.

to_html(ReqData, State) ->
    {"<html><body>I want it all!</body></html>", ReqData, State}.

```

Now our browser will return "I want it all!", but a rest client with a
Content-Type of "application/json" in the request header will still
get our JSON.

But that's dumb. Let's make these content handlers do the job they
were intended to do: format data.

Let's start by moving our album data into the resource's state, which
up until now, didn't even exist.

so we create a state record like this:
```erlang
to_html(ReqData, State) ->
    {"<html><body>I want it all!</body></html>", ReqData, State}.

```

Now our browser will return "I want it all!", but a rest client with a
Content-Type of "application/json" in the request header will still
get our JSON.

But that's dumb. Let's make these content handlers do the job they
were intended to do: format data.

Let's start by moving our album data into the resource's state, which
up until now, didn't even exist.

so we create a state record like this:

```erlang
-record(q_album_resource_state, {
    albums = []
    }).
```

Then we create a new one in `init/1`

```erlang
init([]) ->
    InitialState = #q_album_resource_state{
        albums=[...]
    },
    {{trace, "/tmp/traces"}, InitialState}.
```

Now we need to change our to_json function to pull this info from the
state instead.

```erlang
to_json(ReqData, State=#q_album_resource_state{albums=Albums}) ->
    AlbumsJSON = jiffy:encode(Albums),
    {AlbumsJSON, ReqData, State}.
```

The pattern matching makes that easy!

Now let's make our to_html use the same data

```erlang
to_html(ReqData, State=#q_album_resource_state{albums=Albums}) ->
    HTML = "<html><body>"
    ++ "<h2>Queen Albums</h2>"
    ++ "<ul>"
    ++ [ io_lib:format(
             "<li>~s (~p)</li>~n",
             [ej:get({"name"}, Album), ej:get({"year"}, Album)])
       || Album <- Albums]
    ++ "</ul></body></html>",
    {HTML, ReqData, State}.
```

Great! It's now the same data, but formatted two different ways
depending on which type you asked for.

This is great, but it's all static data. Into each life some
database must fall.

## Postgres.app

I'm not here to teach you to databass. Download
[postgres.app](http://postgresapp.com) and we'll go from there.

Once it's running, click "open psql" and paste in all this junk:

```
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE albums (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name TEXT,
    year integer
);

INSERT INTO albums (name, year) VALUES ('Queen', 1973);
INSERT INTO albums (name, year) VALUES ('Queen II', 1974);
INSERT INTO albums (name, year) VALUES ('Sheer Heart Attack', 1974);
INSERT INTO albums (name, year) VALUES ('A Night at the Opera', 1975);
INSERT INTO albums (name, year) VALUES ('A Day at the Races', 1976);
INSERT INTO albums (name, year) VALUES ('News of the World', 1977);
INSERT INTO albums (name, year) VALUES ('Jazz', 1978);
INSERT INTO albums (name, year) VALUES ('The Game', 1980);
INSERT INTO albums (name, year) VALUES ('Flash Gordon', 1980);
INSERT INTO albums (name, year) VALUES ('Hot Space', 1982);
INSERT INTO albums (name, year) VALUES ('The Works', 1984);
INSERT INTO albums (name, year) VALUES ('A Kind of Magic', 1986);
INSERT INTO albums (name, year) VALUES ('The Miracle', 1989);
INSERT INTO albums (name, year) VALUES ('Innuendo', 1991);
INSERT INTO albums (name, year) VALUES ('Made in Heaven', 1995);
```

Now you have a running database with data in it. Let's teach our
application about it.

## Sqerl and Epgsql

We're going to include `sqerl` which is a lightweight ORM library that
we have. It's more like an *MRM* (module relationship mapping) since
what's an object in erlang anyway?

`sqerl` needs `epgsql` and will just pull it in as a transitive
dependency, so we'll only add sqerl. Actually it pulls in a bunch of things.

```erlang
{deps, [
  {webmachine, "1.10.*",
     {git, "git://github.com/webmachine/webmachine", {branch, "develop"}}},
  {ej, ".*",
     {git, "git://github.com/seth/ej.git", {branch, "master"}}},
  {jiffy, ".*",
     {git, "https://github.com/davisp/jiffy.git", {tag, "0.14.2"}}},
  {sqerl, ".*",
     {git, "https://github.com/chef/sqerl.git", {tag, "1.0.2"}}}
]}.
```

Now we have to configure sqerl to talk to our database. We can do it
in the `./config/sys.config` file that our template made for us.

make it look like this:

```erlang
[
  {'queen', [{web_ip, "0.0.0.0"},
                {web_port, 8080}]},
  {sqerl, [
          %% Database connection parameters
          {db_host, "localhost" },
          {db_port, 5432 },
          {db_user, "" },
          {db_pass, "" },
          {db_name, "" },
          {idle_check, 10000},
          {column_transforms, []},
          {prepared_statements,
            {sqerl_rec, statements, [[{app, queen}]]}}
         ]},
  {pooler, [
    {pools, [[{name, sqerl},
              {max_count,  10 },
              {init_count, 5 },
              {start_mfa, {sqerl_client, start_link, []}}]]}
          ]}
].

```

You'll have to use your own values for `db_user` and `db_name`, both
your mac osx user's short name.

Of course, none of this will load until we add some of these
dependencies we pulled in to your `queen.app.src` file. You just need
to add `sqerl` and `pooler`

```erlang
{application, queen,
 [
  {description, "queen"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  inets,
                  crypto,
                  pooler,
                  sqerl,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { queen_app, []}},
  {env, [
      {web_ip, "0.0.0.0"},
      {web_port, 8080}
  ]}
 ]}.

```

Now if we start a shell, we can ask the database of all of the albums!


```
1> sqerl_rec:fetch_all(queen_album_obj).
[{queen_album_obj,<<"1962125d-5277-4d82-9538-35716900772f">>,
                  <<"Flash Gordon">>,1980},
 {queen_album_obj,<<"1f1a66cd-bf87-4a75-b78f-a4ea8336dae5">>,
                  <<"Sheer Heart Attack">>,1974},
 {queen_album_obj,<<"267bda1d-638e-4bf8-ab06-bad60cb58d6d">>,
                  <<"The Miracle">>,1989},
 {queen_album_obj,<<"40a4b2ce-b160-40d6-b9dc-bc0e7d802294">>,
                  <<"Innuendo">>,1991},
 {queen_album_obj,<<"4cb0f2ef-6740-4c98-a518-2c3d19277d8c">>,
                  <<"Hot Space">>,1982},
 {queen_album_obj,<<"66a7afeb-07b0-4e19-b520-ebf64d17a92a">>,
                  <<"Made in Heaven">>,1995},
 {queen_album_obj,<<"834ad171-b12d-4a86-839b-0b2a8204f111">>,
                  <<"Queen II">>,1974},
 {queen_album_obj,<<"8e4d2fe9-11c0-4754-a566-34890ca58d26">>,
                  <<"A Kind of Magic">>,1986},
 {queen_album_obj,<<"950eea96-d12b-472a-8b8e-aeba8d7094d7">>,
                  <<"Queen">>,1973},
 {queen_album_obj,<<"a14ca841-26d3-44da-994a-b8fa615a0ef4">>,
                  <<"The Game">>,1980},
 {queen_album_obj,<<"a311667c-9989-4c5d-8da1-9c40ed7ec80c">>,
                  <<"News of the World">>,1977},
 {queen_album_obj,<<"cb404e01-ef8d-41ab-a0a3-364d1133db48">>,
                  <<"A Night at the Opera">>,1975},
 {queen_album_obj,<<"d32681f4-6411-43cd-a71c-b98d4ee53177">>,
                  <<"The Works">>,1984},
 {queen_album_obj,<<"f8adda52-6036-45d3-b5b4-9b234efeef4b">>,
                  <<"Jazz">>,1978},
 {queen_album_obj,<<"fa83afc3-6167-45ca-bc5b-9accff2864c9">>,
                  <<"A Day at the Races">>,1976}]
```

And that's cool, but it's a bit different than our data structure we
have in the resource's state.

Fortunately, we have free accessors that were generated from that line
about `sqerl_gobot`.

Let's wire the resource up to the database.

First we just have to change the init function of our resource

```erlang
init([]) ->
    InitialState = #q_album_resource_state{
        albums=sqerl_rec:fetch_all(queen_album_obj)
    },
    {{trace, "/tmp/traces"}, InitialState}.
```

Now we need to modify the to_json and to_html content handlers to use
this new structure

```erlang
to_html(ReqData, State=#q_album_resource_state{albums=Albums}) ->
    HTML = "<html><body>"
    ++ "<h2>Queen Albums</h2>"
    ++ "<ul>"
    ++ [ io_lib:format("<li>~s (~p)</li>~n",
        [
            queen_album_obj:getval('name', Album),
            queen_album_obj:getval('year', Album),
        ])
    || Album <- Albums]
    ++ "</ul></body></html>",
    {HTML, ReqData, State}.
```

to_html is easier because we were already massaging the data
anyway. Let's do to_json now.

```erlang
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
```

We're actually including the uuid in the json output, but not the
HTML. Different strokes for different folks I guess.

The only real problem I see now is that our albums aren't displayed in
chronological order anymore :(

We can fix that for both handlers by sorting the database output.

```erlang
init([]) ->
    Albums = sqerl_rec:fetch_all(queen_album_obj),
    InitialState = #q_album_resource_state{
        albums=lists:sort(
            fun(A,B) ->
                queen_album_obj:getval(year, A) < queen_album_obj:getval(year, B)
            end,
            Albums)
    },
    {{trace, "/tmp/traces"}, InitialState}.
```

But in real life, we'd want to write a query with an order by
clause. We can do it!

All we have to do is revert our change to init and go back into
`queen_album_obj:'#statements'` and change `id` to `year`

```erlang
'#statements'() ->
    [default,
       {fetch_all, sqerl_rec:gen_fetch_all(?MODULE, year)}
    ].
```

## Tests!

I know we should try POSTing some data next, but honestly, I'm sick of
using a browser and rest client to send requests with different
content-types to test things.

We're going to have to write a minature test framework to spin up and
tear down databases between tests. Let's see how we can do it.

First of all, `mkdir test`. Now I'm just going to give you
[pg_test_buddy](./test/pg_test_buddy.erl) for free. Just download it
into your test directory and look at it on your own later. I already
wrote it for another project. I should probably make a generic one
some day.

While we're copying things, look at
[databass.sql](./priv/databass.sql). Copy that into your priv
directory. You'll need it for `pg_test_buddy` to work.

Since this is a web application, we're going to need an HTTP client to
test it. We're going with `ibrowse`. Since we don't need `ibrowse` in
production, just to test, we'll add the dependency to rebar.config's
`test` profile. Your profiles section will look like this now:

```erlang
{profiles, [
    {dev, [
        {deps, [
            {sync, ".*", {git, "git://github.com/rustyio/sync.git", {branch, "master"}}}
        ]}
    ]},
    {test, [
        {deps, [
            {ibrowse, ".*", {git, "https://github.com/cmullaparthi/ibrowse.git", {tag, "v4.1.2"}}}
        ]}
    ]},
    {prod, [
        {relx, [
            {dev_mode, false},
            {include_erts, true}
        ]}
    ]}
]}.
```

Now let's add a test. Create the file `test/basic_test_SUITE.erl`. All
common_test modules end in `_SUITE`, if it doesn't, common_test won't
even find it.

Here's a boilerplate module:

```erlang
-module(basic_test_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() -> [].

init_per_testcase(_, Config) ->
    ibrowse:start(),
    pg_test_buddy:start(Config).

end_per_testcase(_, Config) ->
    pg_test_buddy:stop(Config),
    ok.
```

`init_per_testcase/2` does our per test setup logic. For each test, we
want to start a new postgres with `pg_test_buddy:start/1`

`end_per_testcase/2` does the corresponding teardown.

`all/0` returns a list of `/1` arity functions to run as tests. Since
ours returns `[]`, this suite doesn't even run. LAME! Let's fix it.

We want to test that our HTML output outputs HTML (crazy, right?), so
let's write a test!

```erlang
html(_Config) ->
    {ok, "200", ResponseHeaders, ResponseBody } = ibrowse:send_req(
         "http://localhost:8080/albums", [
        {"Accept", "text/html"}
        ], get),
    ct:pal("Response: ~p", [ResponseBody]),
    [$<,$h,$t,$m,$l|_T] = ResponseBody,
    ok.
```

What's cool about common_test, is that it treats every pattern match
as an assertion. If our ibrowse request returns a non-200 response
code, the pattern match blows up and it's reported in the log.

This code send the request with the `Accept` header of `text/html` and
then we pattern match that the response is a list (aka string) that
starts with an open <html> tag.

It's not a great test, because HTML is meant to be parsed by humans. I
mean we could write an erlang dom walker, but why?

It returns 200, and the response starts with `<html>`. That's good
enough for me.

We still have to add `html` to the `all/0` function, or this won't
run. Once you do, you can go ahead and run it with `rebar3 ct`.

Where's `as test`? well, `ct` and `eunit` commands in rebar3 both
assume the test profile. Isn't that nice of them?

Once that runs you'll see a bunch of output that ultimately ends with
`1 Test Passed`. Yay! If you want to see more, try `open
_build/test/logs/index.html` and your default browser will open an
html report of the entire test run! You'll even see the output from
every `ct:pal` function. See, he really was your pal after all.

Let's add the JSON test now!

```erlang
json(_Config) ->
    {ok, "200", ResponseHeaders, ResponseBody } = ibrowse:send_req(
         "http://localhost:8080/albums", [
        {"Accept", "application/json"}
        ], get),
    ct:pal("Response: ~p", [ResponseBody]),
    JSON = jiffy:decode(ResponseBody),
    15 = length(JSON),
    ok.
```

Same test, but `Accept` is now `application/json` which webmachine is
just going to eat up! Once we get the response, we can parse it with
jiffy. You remember jiffy? well he's back in test form.

Now that we've got decoded json, we can run the length/1 function on
it and see that yes, all 15 of Queen's studio albums are returned.

Again, you still have to add `json` to `all/0`, but once you do, go
ahead and run `rebar3 ct` again.

You can break the tests and run `rebar3 ct` to see them fail if you
like. I won't mind.

## Posting Data

Now, you're probably thinking to yourself: "Freddie Mercury was such
an amazing performer, it's a shame we haven't included any of Queen's
live albums". You're absolutely right. I can't imagine life without
"Live at Wembley '86".

Unfortunately, we've lost our keys to the database, and are going to
need to add these albums via HTTP POST. We're going to want to define
the data structure that we're going to post. We don't know the UUID
for something we haven't added yet, so let's just accept the `name`
and `year` attributes we've accepted before. I'm going to go ahead and
assume `year` is the release year of the album, not the year it was
recorded.

So, let's come up with the payload for "Live at Wembley '86".

```javascript
{
  "name":"Live at Wembley '86",
  "year":1992
}
```

You know what? We can write a common test to test posting this data to
our application before we even teach the application how to handle
it. We're doing TDD in Erlang now!

```erlang
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
    ct:pal("Response: ~p", [ResponseBody2]),
    JSON2 = jiffy:decode(ResponseBody2),
    16 = length(JSON2),
    ok.
```

We do the POST and make sure we're getting 16 albums back now. Of
course we don't. In fact, we get an error when we try to POST. You can
`open _build/test/logs/index.html` to see what happened, but I'll tell
you. We expected a 201, but we got a 405. 405, method not allowed on
this resource. What's up with that?

What's up with that is there's a webmachine callback for what methods
are accepted on the resource. Guess what the default is? Did you say
`GET`? You'd be wrong, it's `GET` and `HEAD`. Let's add `POST` to our
`queen_album_resource` by adding the following function:

```erlang
allowed_methods(Req, State) ->
    {['GET','POST'], Req, State}.
```

All of these callbacks have the same API. They take in a Request
record and the resource state, and they return a 3-tuple of the
callback's answer, the request object, and the resource's state
(possibly modified). I know it can't be modified, it'd be a new state
record that looks suspiciously like the old one. In this case, it is
the old one.

We need to add a few more callbacks before we're in business here, the
first is another simple one.

```erlang
post_is_create(Req, State) ->
    {true, Req, State}.
```

It's just saying that POST means create a new object.

We're also going to need to create the path for this POST, and we're
going to create the UUID in erlang to do it.

We'll need a UUID library for that, so add this to your rebar.config

```erlang
{uuid, ".*",
     {git, "git://github.com/okeuday/uuid.git", {tag, "v1.3.2"}}},
```

and now we'll work on the `create_path/2` callback

```erlang
create_path(Req, State) ->
    Id = uuid:uuid_to_string(uuid:get_v4()),
    {"/albums/" ++ Id, Req, State}.
```

That was easy, and now that will get returned in our `Location` header
for free. Thanks, Webmachine!

We also need to tell our resource that it can ACCEPT the
"application/json" content type.

```erlang
content_types_accepted(Req, State) ->
   {[{"application/json", from_json}], Req, State}.
```

`from_json` is a handler, just like `to_json` so we'll have to
implement it before this works.

```erlang
from_json(Req, State) ->
    %% Create path made us a UUID, let's get it
    ["albums", Id] = string:tokens(wrq:disp_path(Req), "/"),

    %% Read the JSON from the request
    BinJSON = wrq:req_body(Req),
    JSON = jiffy:decode(BinJSON),

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
```
