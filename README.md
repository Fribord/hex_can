
Configuration
=============

# input events

    interface :: integer()

    signal pattern (hex signal pattern)
       { ID::pattern(), Chan::pattern(), Type::pattern(), Value::pattern() }

# output events

    id   ::  integer()      mandatory can id (11 or 29 bit)
    len  ::  0..8           length
    ext  :: boolean()       extended (29 bit) addressing mode
    rtr  :: boolean()       request for transmission
    data :: binary()        binary data 0..8 bytes

