/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_dde.d)
 */
module core.sys.windows.dde;
version (Windows):
pragma(lib, "user32");

private import core.sys.windows.windef;

enum : uint {
    WM_DDE_FIRST     = 0x03E0,
    WM_DDE_INITIATE  = WM_DDE_FIRST,
    WM_DDE_TERMINATE,
    WM_DDE_ADVISE,
    WM_DDE_UNADVISE,
    WM_DDE_ACK,
    WM_DDE_DATA,
    WM_DDE_REQUEST,
    WM_DDE_POKE,
    WM_DDE_EXECUTE,
    WM_DDE_LAST      = WM_DDE_EXECUTE
}

struct DDEACK {
    ubyte bAppReturnCode;
    ubyte _bf;

    @property ubyte reserved() { return cast(ubyte) (_bf & 0x3F); }
    @property bool  fBusy()    { return cast(bool)  (_bf & 0x40); }
    @property bool  fAck()     { return cast(bool)  (_bf & 0x80); }

    @property ubyte reserved(ubyte r) {
        _bf = cast(ubyte) ((_bf & ~0x3F) | (r & 0x3F));
        return cast(ubyte)(r & 0x3F);
    }

    @property bool fBusy(bool f) { _bf = cast(ubyte) ((_bf & ~0x40) | (f << 6)); return f; }
    @property bool fAck(bool f)  { _bf = cast(ubyte) ((_bf & ~0x80) | (f << 7)); return f; }
}

struct DDEADVISE {
    ushort _bf;
    short  cfFormat;

    @property ushort reserved()  { return cast(ushort) (_bf & 0x3FFF); }
    @property bool   fDeferUpd() { return cast(bool)   (_bf & 0x4000); }
    @property bool   fAckReq()   { return cast(bool)   (_bf & 0x8000); }

    @property ushort reserved(ushort r) {
        _bf = cast(ushort) ((_bf & ~0x3FFF) | (r & 0x3FFF));
        return cast(ushort)(r & 0x3FFF);
    }

    @property bool   fDeferUpd(bool f) { _bf = cast(ushort) ((_bf & ~0x4000) | (f << 14)); return f; }
    @property bool   fAckReq(bool f)   { _bf = cast(ushort) ((_bf & ~0x8000) | (f << 15)); return f; }
}

struct DDEDATA {
    ushort _bf;
    short  cfFormat;
    byte   _Value;

    @property ushort unused()    { return cast(ushort) (_bf & 0x0FFF); }
    @property bool   fResponse() { return cast(bool)   (_bf & 0x1000); }
    @property bool   fRelease()  { return cast(bool)   (_bf & 0x2000); }
    @property bool   reserved()  { return cast(bool)   (_bf & 0x4000); }
    @property bool   fAckReq()   { return cast(bool)   (_bf & 0x8000); }

    @property byte*  Value() return { return &_Value; }

    @property ushort unused(ushort r) {
        _bf = cast(ushort) ((_bf & ~0x0FFF) | (r & 0x0FFF));
        return cast(ushort)(r & 0x0FFF);
    }

    @property bool   fResponse(bool f) { _bf = cast(ushort) ((_bf & ~0x1000) | (f << 12)); return f; }
    @property bool   fRelease(bool f)  { _bf = cast(ushort) ((_bf & ~0x2000) | (f << 13)); return f; }
    @property bool   reserved(bool f)  { _bf = cast(ushort) ((_bf & ~0x4000) | (f << 14)); return f; }
    @property bool   fAckReq(bool f)   { _bf = cast(ushort) ((_bf & ~0x8000) | (f << 15)); return f; }
}

struct DDEPOKE {
    ushort _bf;
    short  cfFormat;
    byte   _Value;

    @property ushort unused()    { return cast(ushort) (_bf & 0x1FFF); }
    @property bool   fRelease()  { return cast(bool)   (_bf & 0x2000); }
    @property ubyte  fReserved() { return cast(ubyte)  ((_bf & 0xC000) >>> 14); }

    @property byte*  Value() return { return &_Value; }

    @property ushort unused(ushort u) {
        _bf = cast(ushort) ((_bf & ~0x1FFF) | (u & 0x1FFF));
        return cast(ushort)(u & 0x1FFF);
    }

    @property bool   fRelease(bool f)   { _bf = cast(ushort) ((_bf & ~0x2000) | (f << 13)); return f; }
    @property ubyte  fReserved(ubyte r) { _bf = cast(ushort) ((_bf & ~0xC000) | (r << 14)); return r; }
}

deprecated struct DDELN {
    ushort _bf;
    short  cfFormat;

    @property ushort unused()    { return cast(ushort) (_bf & 0x1FFF); }
    @property bool   fRelease()  { return cast(bool)   (_bf & 0x2000); }
    @property bool   fDeferUpd() { return cast(bool)   (_bf & 0x4000); }
    @property bool   fAckReq()   { return cast(bool)   (_bf & 0x8000); }

    @property ushort unused(ushort u) {
        _bf = cast(ushort)((_bf & ~0x1FFF) | (u & 0x1FFF));
        return cast(ushort)(u & 0x1FFF);
    }

    @property bool   fRelease(bool f)  { _bf = cast(ushort) ((_bf & ~0x2000) | (f << 13)); return f; }
    @property bool   fDeferUpd(bool f) { _bf = cast(ushort) ((_bf & ~0x4000) | (f << 14)); return f; }
    @property bool   fAckReq(bool f)   { _bf = cast(ushort) ((_bf & ~0x8000) | (f << 15)); return f; }
}

deprecated struct DDEUP {
    ushort _bf;
    short  cfFormat;
    byte   _rgb;

    @property ushort unused()    { return cast(ushort) (_bf & 0x0FFF); }
    @property bool   fAck()      { return cast(bool)   (_bf & 0x1000); }
    @property bool   fRelease()  { return cast(bool)   (_bf & 0x2000); }
    @property bool   fReserved() { return cast(bool)   (_bf & 0x4000); }
    @property bool   fAckReq()   { return cast(bool)   (_bf & 0x8000); }

    @property byte*  rgb() return { return &_rgb; }

    @property ushort unused(ushort r) {
        _bf = cast(ushort) ((_bf & ~0x0FFF) | (r & 0x0FFF));
        return cast(ushort)(r & 0x0FFF);
    }

    @property bool   fAck(bool f)      { _bf = cast(ushort) ((_bf & ~0x1000) | (f << 12)); return f; }
    @property bool   fRelease(bool f)  { _bf = cast(ushort) ((_bf & ~0x2000) | (f << 13)); return f; }
    @property bool   fReserved(bool f) { _bf = cast(ushort) ((_bf & ~0x4000) | (f << 14)); return f; }
    @property bool   fAckReq(bool f)   { _bf = cast(ushort) ((_bf & ~0x8000) | (f << 15)); return f; }
}

extern (Windows) {
    BOOL DdeSetQualityOfService(HWND, const(SECURITY_QUALITY_OF_SERVICE)*,
      PSECURITY_QUALITY_OF_SERVICE);
    BOOL ImpersonateDdeClientWindow(HWND, HWND);
    LPARAM PackDDElParam(UINT, UINT_PTR, UINT_PTR);
    BOOL UnpackDDElParam(UINT, LPARAM, PUINT_PTR, PUINT_PTR);
    BOOL FreeDDElParam(UINT, LPARAM);
    LPARAM ReuseDDElParam(LPARAM, UINT, UINT, UINT_PTR, UINT_PTR);
}

debug (WindowsUnitTest) {
    unittest {
        DDEACK ddeack;

        with (ddeack) {
            reserved = 10;
            assert (_bf == 0x0A);
            fBusy = true;
            assert (_bf == 0x4A);
            fAck = true;
            assert (_bf == 0xCA);

            assert (reserved == 10);
            assert (fBusy == true);
            assert (fAck == true);

            reserved = 43;
            assert (_bf == 0xEB);
            fBusy = false;
            assert (_bf == 0xAB);
            fAck = false;
            assert (_bf == 0x2B);

            assert (reserved == 43);
            assert (fBusy == false);
            assert (fAck == false);
        }

        DDEPOKE ddepoke;

        with (ddepoke) {
            unused = 3456;
            assert (_bf == 0x0D80);
            fRelease = true;
            assert (_bf == 0x2D80);
            fReserved = 2;
            assert (_bf == 0xAD80);

            assert (unused == 3456);
            assert (fRelease == true);
            assert (fReserved == 2);

            unused = 2109;
            assert (_bf == 0xa83d);
            fRelease = false;
            assert (_bf == 0x883d);
            fReserved = 1;
            assert (_bf == 0x483d);

            assert (unused == 2109);
            assert (fRelease == false);
            assert (fReserved == 1);
        }
    }
}
