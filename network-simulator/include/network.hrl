%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2016 14:14
%%%-------------------------------------------------------------------
-author("RafalW").

%% Ne device state
-record(state, {
  equipment                   ,
  attr          = #{} :: map(),     %% Attributes of device, KV store
  plugs         = #{}  :: map(),     %% equipped plugs (key: plug id, val: contain record #plug)
  controlPorts  = []  :: pid(),     %% List of control ports (PIDs), e.g where to send events from event log
  routingTable  = #{} :: map(),     %% routing table
  eventLog      = []  :: list(),    %% Log with all events on device, limited list
  eventLogId    = 0   :: integer()  %% Last Event Log Id
}).

%% Plug definition, in and out are PID to another process connected to this plug.
%% Plug can be in following states:
%%    - inactive                  #{in=undefined, out=undefined}
%%    - RX mode (receiving plug)  #{in=PID,       out=undefined}
%%    - TX mode (out plug)        #{in=undefined, out=PID}
%%    - Full Duplex mode          #{in=PID, out=PID}
-record(plug, {id, in, out}).

%% Table with information how to send incoming packets from IN Plug to which OUT Plug.
-record(fiberMap, {}).

%% Static NE data
-record(neData, {name, neType, manId}).

%% NE specification - for starting NE child processes
-record(neSpec, {name, neType}).

%% Event, type
-record(event, {id,
                type, % one of {alm, aud}
                evtInfo,  % #eventInfo
                timestamp}).
-record(eventInfo, {name, old_val, new_val}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%        Messages       %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Heart Beat Message, sent between ne processes.
-record(heart_beat, {from, ttl}).
