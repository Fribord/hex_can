
Configuration
=============

input events
   {interface, I::integer()}

signal pattern (hex signal pattern)
   { ID::pattern(), Chan::pattern(), Type::pattern(), Value::pattern() }

   
output eventes
   {id,  ID::integer()}    mandatory can id
   {len, Len::0..8}        length
   {ext, Ext:boolean()}    extended (29 bit) addressing mode
   {rtr, Rtr::boolean()}   request for transmission
   {data, Data::binary()}  binary data 0..8 bytes
