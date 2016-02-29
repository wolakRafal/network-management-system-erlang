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

## How to run
erl -make

erl -env ERL_LIBS "."

```
1> application:start(network).
```

Script for compilation in shell:

```1> file:eval(".compile_all").```

## Supported Messages (API)

| Operation name  | Input Params  | Return  | Description   |
| :-------------- |:-------------:| ------- | ------------- |
| list_all        |               | ```[NE_DATA]``` |
| get             | ```{ne, NE_ID} ```   |   ```{ok, NE_DATA}```   |
| add             | ```{ne, NE_DATA} ``` |  ``` {ok, {NE_ID, NE_PID}```  |
| remove          | ```{ne, NE_ID}```   |    ``` {ok} ```   |

### Types
  - NE_TYPE : atom()
  - NE_ID : term()
  - NE_DATA: ``` [{ne_id,   NE_ID},
                  {ne_type, NE_TYPE},
                  {ne_pid,  NE_PID},
                 ]
             ```



