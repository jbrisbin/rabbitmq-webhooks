%%% -------------------------------------------------------------------
%%% Author  : J. Brisbin <jon@jbrisbin.com>
%%% Description : RabbitMQ plugin providing webhooks functionality.
%%%
%%% Created : Aug 24, 2010
%%% -------------------------------------------------------------------
-module(rabbit_webhooks_sup).
-author("J. Brisbin <jon@jbrisbin.com>").

-behaviour(supervisor).

-include("rabbit_webhooks.hrl").

-export([start_link/0, init/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
  io:format("Configuring Webhooks...", []),
	Pid = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	io:format("done~n", []),
	Pid.

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	Workers = case application:get_env(webhooks) of
		{ok, W} -> make_worker_configs(W);
		_ -> 
		  io:format("no configs found~n", []),
		  []
	end,
  % One worker per config element  
	{ok, {{one_for_one, 3, 10}, Workers}}.

make_worker_configs(Configs) ->
  lists:foldl(fun (Config, Acc) ->
    case Config of
      {Name, C} ->
        [{Name,
          { rabbit_webhooks, start_link, [Name, C] },
          permanent,
          10000,
          worker,
          [ rabbit_webhooks ]
        } | Acc]
    end
  end, [], Configs).