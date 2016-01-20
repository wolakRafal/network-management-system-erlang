# Network Element (NE) Simulator

Simulates Optical network with Network elements (NE) behaviour.

Each NE contains equipment in hierarchy:
  - Shelfs, 
  - Slots, 
  - Cards, 
  - Plug Holders, 
  - Plugs

This is a somewhat a generic network element implementation,
all equipment inside the device instance is configured (via env attribute).

## Supported Messages

    | Operation name  | Input Params | Return  | Description  |   |
    |-----------------|--------------|---------|--------------|---|
    | list_ne         |              | [{<NE_TYPE>, <NE_DATA>}]  |   |   |
    | get_ne          | <NE_ID>      | not_found OR {<NE_TYPE>, <NE_DATA>}  |   |   |
    | rem_ne          | <NE_ID>      | {ok}     |   |   |

  - listNe(): [{<NE_TYPE>: atom(), <NE_ID>: term()}]
  - getNe(NE_ID): {<NE data>}
  - addNe(<NE data>): {<NE data>}
  - removeNe


## How to run
erl -make

erl -env ERL_LIBS "."
  1> application:start(network).

