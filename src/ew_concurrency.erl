%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-module(ew_concurrency).
-author("Alpha Umaru Shaw").

%% API
-export([least_busy_process/1, least_busy_process/2]).


least_busy_process(Processes) ->
  least_busy_process(Processes, fun(Pid) -> Pid end).


least_busy_process([], _) ->
  false;
least_busy_process([Item], _) ->
  Item;
least_busy_process(Items, PidProvider) when is_list(Items), is_function(PidProvider) ->
  {Processes, TotalQueueLen} = lists:foldl(
      fun(Item, {Acc, Len}) ->
        Pid = PidProvider(Item),
        {message_queue_len, QLen} = erlang:process_info(Pid, message_queue_len),
        {[{Pid, QLen} | Acc], Len + QLen}
      end
    , {[], 0}, Items),
  if TotalQueueLen > 0 ->
    ProcessesCount = length(Processes),
    MeanLen = TotalQueueLen / ProcessesCount,
    {MeanDeviation, StdMeanVariance} = lists:foldl(
      fun({_, QLen}, {MV, SDV}) ->
        IVariance = QLen - MeanLen,
        {(erlang:abs(IVariance) / ProcessesCount) + MV, (math:pow(IVariance, 2) / ProcessesCount) + SDV}
      end, {0, 0}, Processes),
    StdMeanDeviation = math:sqrt(StdMeanVariance),
    error_logger:info_msg(
      "~nQueue Length Stats:~n"
      ++ "\tMean: ~p~n"
         ++ "\tMean Deviation: ~p~n"
            ++ "\tStandard Deviation: ~p",
      [erlwater:round(MeanLen, 3), erlwater:round(MeanDeviation, 3), erlwater:round(StdMeanDeviation, 3)]);
    true -> ok
  end,
  hd(lists:sort(fun({_, QLen1, _}, {_, QLen2, _}) -> QLen1 < QLen2 end, Processes)).