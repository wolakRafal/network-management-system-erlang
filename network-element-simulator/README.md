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

| Operation name  | Input Params  | Return  | Description   |
| :-------------- |:-------------:| ------- | ------------- |
| list_ne         |     p          | ```[{NE_TYPE, NE_DATA}]``` |
| get_ne          | ```NE_ID```         |   ```{NE_TYPE, NE_DATA}```   |
| add_ne          | ``` {NE_TYPE, NE_DATA} ```  |  ``` {ok, NE_ID} ```  |
| rem_ne          | ```NE_ID```         |    ``` {ok} ```   |

### Types
  - NY_TYPE : atom()
  - NY_ID : term()

## How to run
erl -make

erl -env ERL_LIBS "."
  1> application:start(network).

