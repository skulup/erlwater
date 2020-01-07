%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-module(erlwater_net).
-author("Alpha Umaru Shaw").

-include("erlwater.hrl").

%% API
-export([ip_to_binary/1, get_mac_address/0]).


-spec ip_to_binary(inet:ip_address() | undefined |
{inet:ip_address(), inet:port_number()}) -> binary().
ip_to_binary({IP, _Port}) ->
  ip_to_binary(IP);
ip_to_binary(?Undef) ->
  <<"undefined">>;
ip_to_binary(Ip) ->
  list_to_binary(inet:ntoa(Ip)).



get_mac_address() ->
  {ok, Ls} = inet:getifaddrs(),
  IFaceProps = hd([Props || {Inf, Props} <- Ls,
    Inf /= "lo", filter_interfaces(Props)]),
  case proplists:get_value(hwaddr, IFaceProps) of
    undefined -> {error, no_mac_address};
    Addr ->
      list_to_binary(Addr)
  end.


filter_interfaces(Props) ->
  case proplists:get_value(hwaddr, Props) of
    undefined -> false;
    [0, 0, 0, 0, 0, 0] -> false;
    _ -> true
  end.
