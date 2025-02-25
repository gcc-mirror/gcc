@safe unittest
{
    import std.datetime.timezone;

        version (Posix)
        {
            auto tz = PosixTimeZone.getTimeZone("America/Los_Angeles");

            assert(tz.name == "America/Los_Angeles");
            assert(tz.stdName == "PST");
            assert(tz.dstName == "PDT");
        }
    
}

