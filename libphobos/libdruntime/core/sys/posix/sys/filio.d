/**
 * D header file for POSIX.
 *
 * License: $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 */

module core.sys.posix.sys.filio;

import core.sys.posix.sys.ioccom;

version (Posix):

nothrow @nogc:

version (OSX)
{
    // File-descriptor ioctl's
    enum uint FIOCLEX   = _IO('f', 1);         // set close on exec on fd
    enum uint FIONCLEX  = _IO('f', 2);         // remove close on exec
    enum uint FIONREAD  = _IOR!(int)('f', 127); // get # bytes to read
    enum uint FIONBIO   = _IOW!(int)('f', 126); // set/clear non-blocking i/o
    enum uint FIOASYNC  = _IOW!(int)('f', 125); // set/clear async i/o
    enum uint FIOSETOWN = _IOW!(int)('f', 124); // set owner
    enum uint FIOGETOWN = _IOR!(int)('f', 123); // get owner
    enum uint FIODTYPE  = _IOR!(int)('f', 122); // get d_type
}
