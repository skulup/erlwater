%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-module(ew_net).
-author("Alpha Umaru Shaw").

-include("erlwater.hrl").
-include_lib("kernel/include/inet.hrl").

%% API
-export([ip_to_binary/1, get_mac_address/0, activate_socket/1, optimize_socket/1]).
-export([logical_to_physical_address/1, logical_to_physical_addresses/1]).


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

activate_socket(Sock) ->
  ok = inet:setopts(Sock, [{active, true}]),
  Sock.

optimize_socket(Sock) ->
  {ok, [{sndbuf, SndBufferSize}, {recbuf, RecBufferSize}]} =
  inet:getopts(Sock, [sndbuf, recbuf]), %% assert
  ok = inet:setopts(Sock, [{buffer, max(RecBufferSize, SndBufferSize)}]),
  Sock.


logical_to_physical_address(Address) ->
  case logical_to_physical_addresses(Address) of
    [] -> {error, address_not_available};
    [H | _] -> H;
    Else -> Else
  end.

logical_to_physical_addresses(Address) when is_binary(Address) ->
  logical_to_physical_addresses(binary_to_list(Address));
logical_to_physical_addresses(Address) when is_list(Address) ->
  case resolve_address(Address) of
    {error, Reason} ->
      {error, {Reason, Address}};
    {_, _, Addresses} ->
      Addresses
  end.

%%%%%%%%%%%%%%%%%%%%%%%
%%%     Internal    %%%
%%%%%%%%%%%%%%%%%%%%%%%

resolve_address(Host) ->
  Host1 = erlwater:trim(Host),
  case resolve_address1(Host1) of
    {error, _} = Err ->
      Err;
    {Hostname, AddressType, Addresses} ->
      {Hostname, AddressType, Addresses}
  end.

resolve_address1(Hostname) ->
  case inet:gethostbyname(Hostname) of
    {error, _} = Err ->
      Err;
    {ok, #hostent{h_name = Host, h_addrtype = AddressType, h_addr_list = Addresses}} ->
      {Host, AddressType, Addresses}
  end.