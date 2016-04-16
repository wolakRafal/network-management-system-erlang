%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo
%%% @doc
%%%
%%% @end
%%% Created : 12. sty 2016 19:59
%%%-------------------------------------------------------------------
{application, network, [
  {description, "Optical Network Simulator"},
  {vsn, "0.1.0"},
  {registered, [network, md1]},
  {applications, [
    kernel,
    stdlib
  ]},
  {modules, [equipment,ne_device,network,network_sup]},
  {mod, {network, [
    %% { management_domain, md1},
    %% ne record { ne_name, ne_type,
    {ne_list, []} %% change to file location with preset NE configurations
  ]}},
  %% here will go defaults (static caps)
  {env, [ ]}
]}.