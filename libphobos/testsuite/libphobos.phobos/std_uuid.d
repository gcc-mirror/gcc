@safe unittest
{
    import std.uuid;

    import std.uuid;

    UUID[] ids;
    ids ~= randomUUID();
    ids ~= md5UUID("test.name.123");
    ids ~= sha1UUID("test.name.123");

    foreach (entry; ids)
    {
        assert(entry.variant == UUID.Variant.rfc4122);
    }
    assert(ids[0].uuidVersion == UUID.Version.randomNumberBased);
    assert(ids[1].toString() == "22390768-cced-325f-8f0f-cfeaa19d0ccd");
    assert(ids[1].data == [34, 57, 7, 104, 204, 237, 50, 95, 143, 15, 207,
        234, 161, 157, 12, 205]);
    UUID id;
    assert(id.empty);
}

@safe pure unittest
{
    import std.uuid;

            enum ubyte[16] data = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
            auto uuid = UUID(data);
            enum ctfe = UUID(data);
            assert(uuid.data == data);
            assert(ctfe.data == data);
        
}

@safe unittest
{
    import std.uuid;

            auto tmp = UUID(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
            assert(tmp.data == cast(ubyte[16])[0,1,2,3,4,5,6,7,8,9,10,11,
                12,13,14,15]);
        
}

@safe pure unittest
{
    import std.uuid;

            auto id = UUID("8AB3060E-2cba-4f23-b74c-b52db3bdfb46");
            assert(id.data == [138, 179, 6, 14, 44, 186, 79, 35, 183, 76,
               181, 45, 179, 189, 251, 70]);
            assert(id.toString() == "8ab3060e-2cba-4f23-b74c-b52db3bdfb46");

            //Can also be used in CTFE, for example as UUID literals:
            enum ctfeID = UUID("8ab3060e-2cba-4f23-b74c-b52db3bdfb46");
            //here parsing is done at compile time, no runtime overhead!
        
}

@safe pure unittest
{
    import std.uuid;

            UUID id;
            assert(id.empty);
            id = UUID("00000000-0000-0000-0000-000000000001");
            assert(!id.empty);
        
}

@safe pure unittest
{
    import std.uuid;

            assert(UUID("8ab3060e-2cba-4f23-b74c-b52db3bdfb46").variant
               == UUID.Variant.rfc4122);
        
}

@safe unittest
{
    import std.uuid;

            assert(UUID("8ab3060e-2cba-4f23-b74c-b52db3bdfb46").uuidVersion
                == UUID.Version.randomNumberBased);
        
}

@safe unittest
{
    import std.uuid;

            immutable ubyte[16] data = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
            UUID u1;
            UUID u2 = UUID(data);
            u1.swap(u2);

            assert(u1 == UUID(data));
            assert(u2 == UUID.init);
        
}

@safe pure unittest
{
    import std.uuid;

            //compare UUIDs
            assert(UUID("00000000-0000-0000-0000-000000000000") == UUID.init);

            //UUIDs in associative arrays:
            int[UUID] test = [UUID("8a94f585-d180-44f7-8929-6fca0189c7d0") : 1,
                UUID("7c351fd4-b860-4ee3-bbdc-7f79f3dfb00a") : 2,
                UUID("9ac0a4e5-10ee-493a-86fc-d29eeb82ecc1") : 3];

            assert(test[UUID("9ac0a4e5-10ee-493a-86fc-d29eeb82ecc1")] == 3);

            //UUIDS can be sorted:
            import std.algorithm;
            UUID[] ids = [UUID("8a94f585-d180-44f7-8929-6fca0189c7d0"),
                          UUID("7c351fd4-b860-4ee3-bbdc-7f79f3dfb00a"),
                          UUID("9ac0a4e5-10ee-493a-86fc-d29eeb82ecc1")];
            sort(ids);
        
}

@safe pure unittest
{
    import std.uuid;

            immutable str = "8ab3060e-2cba-4f23-b74c-b52db3bdfb46";
            auto id = UUID(str);
            assert(id.toString() == str);
        
}

@safe unittest
{
    import std.uuid;

    UUID id;
    assert(id.empty);

    id = randomUUID;
    assert(!id.empty);

    id = UUID(cast(ubyte[16]) [138, 179, 6, 14, 44, 186, 79,
        35, 183, 76, 181, 45, 179, 189, 251, 70]);
    assert(id.toString() == "8ab3060e-2cba-4f23-b74c-b52db3bdfb46");
}

@safe unittest
{
    import std.uuid;

    //Use default UUID.init namespace
    auto simpleID = md5UUID("test.uuid.any.string");

    //use a name-based id as namespace
    auto namespace = md5UUID("my.app");
    auto id = md5UUID("some-description", namespace);
}

@safe unittest
{
    import std.uuid;

    //Use default UUID.init namespace
    auto simpleID = sha1UUID("test.uuid.any.string");

    //use a name-based id as namespace
    auto namespace = sha1UUID("my.app");
    auto id = sha1UUID("some-description", namespace);
}

@safe unittest
{
    import std.uuid;

    import std.random : Xorshift192, unpredictableSeed;

    //simple call
    auto uuid = randomUUID();

    //provide a custom RNG. Must be seeded manually.
    Xorshift192 gen;

    gen.seed(unpredictableSeed);
    auto uuid3 = randomUUID(gen);
}

@safe unittest
{
    import std.uuid;

    auto id = parseUUID("8AB3060E-2CBA-4F23-b74c-B52Db3BDFB46");
    //no dashes
    id = parseUUID("8ab3060e2cba4f23b74cb52db3bdfb46");
    //dashes at different positions
    id = parseUUID("8a-b3-06-0e2cba4f23b74c-b52db3bdfb-46");
    //leading / trailing characters
    id = parseUUID("{8ab3060e-2cba-4f23-b74c-b52db3bdfb46}");
    //unicode
    id = parseUUID("ü8ab3060e2cba4f23b74cb52db3bdfb46ü");
    //multiple trailing/leading characters
    id = parseUUID("///8ab3060e2cba4f23b74cb52db3bdfb46||");

    //Can also be used in CTFE, for example as UUID literals:
    enum ctfeID = parseUUID("8ab3060e-2cba-4f23-b74c-b52db3bdfb46");
    //here parsing is done at compile time, no runtime overhead!
}

@safe unittest
{
    import std.uuid;

    import std.algorithm;
    import std.regex;

    string test = "Lorem ipsum dolor sit amet, consetetur "~
    "6ba7b814-9dad-11d1-80b4-00c04fd430c8 sadipscing \n"~
    "elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore \r\n"~
    "magna aliquyam erat, sed diam voluptua. "~
    "8ab3060e-2cba-4f23-b74c-b52db3bdfb46 At vero eos et accusam et "~
    "justo duo dolores et ea rebum.";

    auto r = regex(uuidRegex, "g");
    UUID[] found;
    foreach (c; match(test, r))
    {
        found ~= UUID(c.hit);
    }
    assert(found == [
        UUID("6ba7b814-9dad-11d1-80b4-00c04fd430c8"),
        UUID("8ab3060e-2cba-4f23-b74c-b52db3bdfb46"),
    ]);
}

@safe unittest
{
    import std.uuid;

    import std.exception : collectException;

    const inputUUID = "this-is-an-invalid-uuid";
    auto ex = collectException!UUIDParsingException(UUID(inputUUID));
    assert(ex !is null); // check that exception was thrown
    assert(ex.input == inputUUID);
    assert(ex.position == 0);
    assert(ex.reason == UUIDParsingException.Reason.tooLittle);
}

