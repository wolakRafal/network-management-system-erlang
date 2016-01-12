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
  {mod, {network, [
   %% { management_domain, md1},
    %% ne record { ne-name, ne-type, ne-ip, self-uri
    { ne_list, [{"f7_1", f7, "10.192.0.1" , "/md/1/me/1"}] }
  ]}},
  %% here will go defaults (static caps)
  {env, [ ]}
]}.