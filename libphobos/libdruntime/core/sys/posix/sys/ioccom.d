/**
 * D header file for POSIX.
 *
 * License: $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 */

module core.sys.posix.sys.ioccom;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):

nothrow @nogc:

version (Darwin)
{
    /* OSX ioctl's (based on FreeBSD) encode the command in the lower 16-bits
     * and the size of any in/out parameters in the lower 13 bits of the upper
     * 16-bits of a 32 bit unsigned integer. The high 3 bits of the upper
     * 16-bits encode the in/out status of the parameter.
     */
    enum uint IOCPARM_MASK = 0x1fff; // parameter length, at most 13 bits
    uint IOCPARM_LEN(uint x) // to extract the encoded parameter length
    {
        return ((x >> 16) & IOCPARM_MASK);
    }
    uint IOCBASECMD(uint x) // to extract the encoded command
    {
        return (x & ~(IOCPARM_MASK << 16));
    }
    uint IOCGROUP(uint x) // to extract the encoded group
    {
        return ((x >> 8) & 0xff);
    }

    enum uint IOCPARM_MAX = (IOCPARM_MASK + 1); // max size of ioctl args

    enum uint IOC_VOID = 0x20000000; // no parameters
    enum uint IOC_OUT = 0x40000000; // copy parameters back
    enum uint IOC_IN = 0x80000000; // copy parameters into
    enum uint IOC_INOUT = (IOC_IN | IOC_OUT); // copy parameter into and get back
    enum uint IOC_DIRMASK = 0xe0000000; // mask to extract above direction parameters

    // encode the ioctl info into 32 bits
    uint _IOC(T=typeof(null))(uint inorout, uint group, uint num, size_t len)
    {
        return (inorout | ((len & IOCPARM_MASK) << 16) | (group << 8) | num);
    }

    // encode a command with no parameters
    uint _IO(char g, int n)
    {
        return _IOC(IOC_VOID, cast(uint)g, cast(uint)n, cast(size_t)0);
    }
    // encode a command that returns info
    uint _IOR(T)(char g, int n)
    {
        return _IOC!(T)(IOC_OUT, cast(uint)g, cast(uint)n, T.sizeof);
    }
    // encode a command that takes info
    uint _IOW(T)(char g, int n)
    {
        return _IOC!(T)(IOC_IN, cast(uint)g, cast(uint)n, T.sizeof);
    }
    // encode a command that takes info and returns info
    uint _IOWR(T)(char g, int n)
    {
        return _IOC!(T)(IOC_INOUT, cast(uint)g, cast(uint)n, T.sizeof);
    }
}
else version (FreeBSD)
{
    /* FreeBSD ioctl's encode the command in the lower 16-bits
     * and the size of any in/out parameters in the lower 13 bits of the upper
     * 16-bits of a 32 bit unsigned integer. The high 3 bits of the upper
     * 16-bits encode the in/out status of the parameter.
     */
    enum uint IOCPARM_SHIFT = 13; // number of bits for ioctl size
    enum uint IOCPARM_MASK = ((1 << IOCPARM_SHIFT) - 1); // parameter length mask
    uint IOCPARM_LEN(uint x) // to extract the encoded parameter length
    {
        return ((x >> 16) & IOCPARM_MASK);
    }
    uint IOCBASECMD(uint x) // to extract the encoded command
    {
        return (x & ~(IOCPARM_MASK << 16));
    }
    uint IOCGROUP(uint x) // to extract the encoded group
    {
        return ((x >> 8) & 0xff);
    }

    enum uint IOCPARM_MAX = (1 << IOCPARM_SHIFT); // max size of ioctl args

    enum uint IOC_VOID = 0x20000000; // no parameters
    enum uint IOC_OUT = 0x40000000; // copy parameters back
    enum uint IOC_IN = 0x80000000; // copy parameters into
    enum uint IOC_INOUT = (IOC_IN | IOC_OUT);
    enum uint IOC_DIRMASK = (IOC_VOID|IOC_OUT|IOC_IN);

    // encode the ioctl info into 32 bits
    uint _IOC(uint inorout, uint group, uint num, size_t len)
    {
        return (inorout | ((len & IOCPARM_MASK) << 16) | (group << 8) | num);
    }

    // encode a command with no parameters
    uint _IO(char g, int n)
    {
        return _IOC(IOC_VOID, cast(uint)g, cast(uint)n, cast(size_t)0);
    }
    uint _IOWINT(char g, int n)
    {
        return _IOC(IOC_VOID, cast(uint)g, cast(uint)n, int.sizeof);
    }
    // encode a command that returns info
    uint _IOR(T)(char g, int n)
    {
        return _IOC(IOC_OUT, cast(uint)g, cast(uint)n, T.sizeof);
    }
    // encode a command that takes info
    uint _IOW(T)(char g, int n)
    {
        return _IOC(IOC_IN, cast(uint)g, cast(uint)n, T.sizeof);
    }
    // encode a command that takes info and returns info
    uint _IOWR(T)(char g, int n)
    {
        return _IOC(IOC_INOUT, cast(uint)g, cast(uint)n, T.sizeof);
    }
}
