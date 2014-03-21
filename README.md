
Configuration
=============

# input events

    [{id,unsigned32()},{mask,unsigned32()},{invert,boolean()},
     {intf,integer()}].

## example

    %% can input from interface 1 id=xxx20002
    {event, can1,
    	    {hex_can, [{id,16#20002},{mask,16#20002},{intf,1}]},
	    {id, chan, type, value}}.

The above signal spec contains references to environment positions
to construct the hex signal from a can frame.

# output events

    [{id, unsigned32()},    mandatory can id (11 or 29 bit)
     {len, -1..8},          optional length
     {ext, boolean()},      optional extended (29 bit) addressing mode
     {rtr, boolean()},      optional request for transmission
     {data, binary()}       optional binary data 0..8 bytes
     {intf, integer()}]     optional interface number
