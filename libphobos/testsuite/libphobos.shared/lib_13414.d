shared void function() sharedStaticDtorHook;
shared void function() staticDtorHook;
shared static ~this() { sharedStaticDtorHook(); }
static ~this() { staticDtorHook(); }
