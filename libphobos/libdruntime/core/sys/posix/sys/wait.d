/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly, Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.sys.wait;

import core.sys.posix.config;
public import core.sys.posix.sys.types; // for id_t, pid_t
public import core.sys.posix.signal;    // for siginfo_t (XSI)
//public import core.sys.posix.resource; // for rusage (XSI)

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C) nothrow @nogc:
@system:

//
// Required
//
/*
WNOHANG
WUNTRACED

WEXITSTATUS
WIFCONTINUED
WIFEXITED
WIFSIGNALED
WIFSTOPPED
WSTOPSIG
WTERMSIG

pid_t wait(int*);
pid_t waitpid(pid_t, int*, int);
*/

version (CRuntime_Glibc)
{
    @safe pure:

    enum WNOHANG        = 1;
    enum WUNTRACED      = 2;

    private
    {
        enum __W_CONTINUED = 0xFFFF;

        extern (D) int __WTERMSIG( int status ) { return status & 0x7F; }
    }

    //
    // NOTE: These macros assume __USE_MISC is not defined in the relevant
    //       C headers as the parameter definition there is different and
    //       much more complicated.
    //
    extern (D) int  WEXITSTATUS( int status )  { return ( status & 0xFF00 ) >> 8;   }
    extern (D) int  WIFCONTINUED( int status ) { return status == __W_CONTINUED;    }
    extern (D) bool WIFEXITED( int status )    { return __WTERMSIG( status ) == 0;  }
    extern (D) bool WIFSIGNALED( int status )
    {
        return ( cast(byte) ( ( status & 0x7F ) + 1 ) >> 1 ) > 0;
    }
    extern (D) bool WIFSTOPPED( int status )   { return ( status & 0xFF ) == 0x7F;  }
    extern (D) int  WSTOPSIG( int status )     { return WEXITSTATUS( status );      }
    extern (D) int  WTERMSIG( int status )     { return status & 0x7F;              }
}
else version (Darwin)
{
    @safe pure:

    enum WNOHANG        = 1;
    enum WUNTRACED      = 2;

    private
    {
        enum _WSTOPPED = 0x7F; // octal 0177
    }

    extern (D) int _WSTATUS(int status)         { return (status & 0x7F);           }
    extern (D) int  WEXITSTATUS( int status )   { return (status >> 8);             }
    extern (D) int  WIFCONTINUED( int status )  { return status == 0x13;            }
    extern (D) bool WIFEXITED( int status )     { return _WSTATUS(status) == 0;     }
    extern (D) bool WIFSIGNALED( int status )
    {
        return _WSTATUS( status ) != _WSTOPPED && _WSTATUS( status ) != 0;
    }
    extern (D) bool WIFSTOPPED( int status )   { return _WSTATUS( status ) == _WSTOPPED; }
    extern (D) int  WSTOPSIG( int status )     { return status >> 8;                     }
    extern (D) int  WTERMSIG( int status )     { return _WSTATUS( status );              }
}
else version (FreeBSD)
{
    @safe pure:

    enum WNOHANG        = 1;
    enum WUNTRACED      = 2;

    private
    {
        enum _WSTOPPED = 0x7F; // octal 0177
    }

    extern (D) int _WSTATUS(int status)         { return (status & 0x7F);           }
    extern (D) int  WEXITSTATUS( int status )   { return (status >> 8);             }
    extern (D) int  WIFCONTINUED( int status )  { return status == 0x13;            }
    extern (D) bool WIFEXITED( int status )     { return _WSTATUS(status) == 0;     }
    extern (D) bool WIFSIGNALED( int status )
    {
        return _WSTATUS( status ) != _WSTOPPED && _WSTATUS( status ) != 0;
    }
    extern (D) bool WIFSTOPPED( int status )   { return _WSTATUS( status ) == _WSTOPPED; }
    extern (D) int  WSTOPSIG( int status )     { return status >> 8;                     }
    extern (D) int  WTERMSIG( int status )     { return _WSTATUS( status );              }
}
else version (NetBSD)
{
    @safe pure:

    enum WNOHANG        = 1;
    enum WUNTRACED      = 2;

    private
    {
        enum _WSTOPPED = 0x7F; // octal 0177
    }

    extern (D) int _WSTATUS(int status)         { return (status & 0x7F);           }
    extern (D) int  WEXITSTATUS( int status )   { return (status >> 8);             }
    extern (D) int  WIFCONTINUED( int status )  { return status == 0x13;            }
    extern (D) bool WIFEXITED( int status )     { return _WSTATUS(status) == 0;     }
    extern (D) bool WIFSIGNALED( int status )
    {
        return _WSTATUS( status ) != _WSTOPPED && _WSTATUS( status ) != 0;
    }
    extern (D) bool WIFSTOPPED( int status )   { return _WSTATUS( status ) == _WSTOPPED; }
    extern (D) int  WSTOPSIG( int status )     { return status >> 8;                     }
    extern (D) int  WTERMSIG( int status )     { return _WSTATUS( status );              }
}
else version (OpenBSD)
{
    @safe pure:

    enum WNOHANG        = 1;
    enum WUNTRACED      = 2;

    private
    {
        enum _WSTOPPED   = 0x7F;   // octal 0177
        enum _WCONTINUED = 0xFFFF; // octal 0177777
    }

    extern (D) int _WSTATUS(int status)         { return (status & 0x7F);                     }
    extern (D) int  WEXITSTATUS(int status)   { return (status >> 8) & 0xFF;                  }
    extern (D) int  WIFCONTINUED(int status)  { return (status & _WCONTINUED) == _WCONTINUED; }
    extern (D) bool WIFEXITED(int status)     { return _WSTATUS(status) == 0;                 }
    extern (D) bool WIFSIGNALED(int status)
    {
        return _WSTATUS(status) != _WSTOPPED && _WSTATUS(status) != 0;
    }
    extern (D) bool WIFSTOPPED(int status)   { return (status & 0xFF) == _WSTOPPED; }
    extern (D) int  WSTOPSIG(int status)     { return (status >> 8) & 0xFF;         }
    extern (D) int  WTERMSIG(int status)     { return _WSTATUS(status);             }
}
else version (DragonFlyBSD)
{
    @safe pure:

    enum WNOHANG        = 1;
    enum WUNTRACED      = 2;

    private
    {
        enum _WSTOPPED = 0x7F; // octal 0177
    }

    extern (D) int _WSTATUS(int status)         { return (status & 0x7F);           }
    extern (D) int  WEXITSTATUS( int status )   { return (status >> 8);             }
    extern (D) int  WIFCONTINUED( int status )  { return status == 0x13;            }
    extern (D) bool WIFEXITED( int status )     { return _WSTATUS(status) == 0;     }
    extern (D) bool WIFSIGNALED( int status )
    {
        return _WSTATUS( status ) != _WSTOPPED && _WSTATUS( status ) != 0;
    }
    extern (D) bool WIFSTOPPED( int status )   { return _WSTATUS( status ) == _WSTOPPED; }
    extern (D) int  WSTOPSIG( int status )     { return status >> 8;                     }
    extern (D) int  WTERMSIG( int status )     { return _WSTATUS( status );              }
}
else version (Solaris)
{
    @safe pure:

    enum WNOHANG        = 64;
    enum WUNTRACED      = 4;

    extern (D) int WEXITSTATUS(int status) { return (status >> 8) & 0xff; }
    extern (D) int WIFCONTINUED(int status) { return (status & 0xffff) == 0xffff; }
    extern (D) bool WIFEXITED(int status) { return (status & 0xff) == 0;     }
    extern (D) bool WIFSIGNALED(int status) { return (status & 0xff) > 0 && (status & 0xff00) == 0; }
    extern (D) bool WIFSTOPPED(int status) { return (status & 0xff) == 0x7f && (status & 0xff00) != 0; }
    extern (D) int WSTOPSIG(int status) { return (status >> 8) & 0x7f; }
    extern (D) int WTERMSIG(int status) { return (status & 0x7f); }
}
else version (CRuntime_Bionic)
{
    @safe pure:

    enum WNOHANG   = 1;
    enum WUNTRACED = 2;

    extern (D) int  WEXITSTATUS( int status ) { return ( status & 0xFF00 ) >> 8; }
    extern (D) bool WIFEXITED( int status ) { return WTERMSIG(status) == 0; }
    extern (D) bool WIFSIGNALED( int status ) { return WTERMSIG(status + 1) >= 2; }
    extern (D) bool WIFSTOPPED( int status ) { return WTERMSIG(status) == 0x7F; }
    extern (D) int  WSTOPSIG( int status ) { return WEXITSTATUS(status); }
    extern (D) int  WTERMSIG( int status ) { return status & 0x7F; }
}
else version (CRuntime_Musl)
{
    @safe pure:

    enum WNOHANG        = 1;
    enum WUNTRACED      = 2;

    extern (D) int  WEXITSTATUS( int status ) { return ( status & 0xFF00 ) >> 8; }
    extern (D) int  WIFCONTINUED( int status ) { return status == 0xffff; }
    extern (D) bool WIFEXITED( int status ) { return WTERMSIG( status ) == 0; }
    extern (D) bool WIFSIGNALED( int status ) { return (status&0xffff)-1U < 0xffU; }
    extern (D) bool WIFSTOPPED( int status ) { return cast(short)(((status&0xffff)*0x10001)>>8) > 0x7f00; }
    extern (D) int  WTERMSIG( int status ) { return status & 0x7F; }
    alias WEXITSTATUS WSTOPSIG;
}
else version (CRuntime_UClibc)
{
    @safe pure:

    enum WNOHANG        = 1;
    enum WUNTRACED      = 2;

    private
    {
        enum __W_CONTINUED = 0xFFFF;

        extern (D) int __WTERMSIG( int status ) { return status & 0x7F; }
    }

    //
    // NOTE: These macros assume __USE_BSD is not defined in the relevant
    //       C headers as the parameter definition there is different and
    //       much more complicated.
    //
    extern (D) int  WEXITSTATUS( int status )  { return ( status & 0xFF00 ) >> 8;   }
    extern (D) int  WIFCONTINUED( int status ) { return status == __W_CONTINUED;    }
    extern (D) bool WIFEXITED( int status )    { return __WTERMSIG( status ) == 0;  }
    extern (D) bool WIFSIGNALED( int status )
    {
        return ( cast(ulong) ( ( status & 0xffff ) - 1U ) >> 1 ) < 0xffU;
    }
    version (MIPS32)
    {
        extern (D) bool WIFSTOPPED( int status )   { return ( status & 0xFF ) == 0x7F;  }
    }
    else
    {
        extern (D) bool WIFSTOPPED( int status )   { return ( status & 0xFF ) == 0x7F && ( status & 0xFF00 );  }
    }
    extern (D) int  WSTOPSIG( int status )     { return WEXITSTATUS( status );      }
    extern (D) int  WTERMSIG( int status )     { return status & 0x7F;              }
}
else
{
    static assert(false, "Unsupported platform");
}

pid_t wait(int*);
pid_t waitpid(pid_t, int*, int);

//
// XOpen (XSI)
//
/*
WEXITED
WSTOPPED
WCONTINUED
WNOWAIT

enum idtype_t
{
    P_ALL,
    P_PID,
    P_PGID
}

int waitid(idtype_t, id_t, siginfo_t*, int);
*/

version (CRuntime_Glibc)
{
    enum WEXITED    = 4;
    enum WSTOPPED   = 2;
    enum WCONTINUED = 8;
    enum WNOWAIT    = 0x01000000;

    enum idtype_t
    {
        P_ALL,
        P_PID,
        P_PGID
    }

    int waitid(idtype_t, id_t, siginfo_t*, int);
}
else version (Darwin)
{
    enum WEXITED    = 0x00000004;
    enum WSTOPPED   = 0x00000008;
    enum WCONTINUED = 0x00000010;
    enum WNOWAIT    = 0x00000020;

    enum idtype_t
    {
        P_ALL,
        P_PID,
        P_PGID
    }

    int waitid(idtype_t, id_t, siginfo_t*, int);
}
else version (FreeBSD)
{
    enum WSTOPPED       = WUNTRACED;
    enum WCONTINUED     = 4;
    enum WNOWAIT        = 8;
    enum WEXITED        = 16;
    enum WTRAPPED       = 32;

    enum idtype_t
    {
        P_UID,
        P_GID,
        P_SID,
        P_JAILID,
        P_PID,
        P_PPID,
        P_PGID,
        P_CID,
        P_ALL,
        P_LWPID,
        P_TASKID,
        P_PROJID,
        P_POOLID,
        P_CTID,
        P_CPUID,
        P_PSETID
    }

    int waitid(idtype_t, id_t, siginfo_t*, int);
}
else version (NetBSD)
{
    enum WSTOPPED       = WUNTRACED;
    //enum WCONTINUED     = 4;
    enum WNOWAIT        = 0x00010000;
}
else version (OpenBSD)
{
    enum WCONTINUED     = 8;
    // OpenBSD does not define the following:
    //enum WSTOPPED
    //enum WNOWAIT
}
else version (DragonFlyBSD)
{
    enum WSTOPPED       = WUNTRACED;
    enum WCONTINUED     = 4;
    enum WNOWAIT        = 8;
}
else version (Solaris)
{
    enum WEXITED = 1;
    enum WTRAPPED = 2;
    enum WSTOPPED = WUNTRACED;
    enum WCONTINUED = 8;
    enum WNOWAIT = 128;

    enum idtype_t
    {
        P_PID,          /* A process identifier.                */
        P_PPID,         /* A parent process identifier.         */
        P_PGID,         /* A process group (job control group)  */
                        /* identifier.                          */
        P_SID,          /* A session identifier.                */
        P_CID,          /* A scheduling class identifier.       */
        P_UID,          /* A user identifier.                   */
        P_GID,          /* A group identifier.                  */
        P_ALL,          /* All processes.                       */
        P_LWPID,        /* An LWP identifier.                   */
        P_TASKID,       /* A task identifier.                   */
        P_PROJID,       /* A project identifier.                */
        P_POOLID,       /* A pool identifier.                   */
        P_ZONEID,       /* A zone identifier.                   */
        P_CTID,         /* A (process) contract identifier.     */
        P_CPUID,        /* CPU identifier.                      */
        P_PSETID,       /* Processor set identifier             */
    }

    int waitid(idtype_t, id_t, siginfo_t*, int);
}
else version (CRuntime_Bionic)
{
    enum WEXITED    = 4;
    enum WSTOPPED   = 2;
    enum WCONTINUED = 8;
    enum WNOWAIT    = 0x01000000;

    alias int idtype_t;

    int waitid(idtype_t, id_t, siginfo_t*, int);
}
else version (CRuntime_Musl)
{
    enum WEXITED    = 4;
    enum WSTOPPED   = 2;
    enum WCONTINUED = 8;
    enum WNOWAIT    = 0x01000000;

    enum idtype_t
    {
        P_ALL,
        P_PID,
        P_PGID
    }

    int waitid(idtype_t, id_t, siginfo_t*, int);
}
else version (CRuntime_UClibc)
{
    enum WEXITED    = 4;
    enum WSTOPPED   = 2;
    enum WCONTINUED = 8;
    enum WNOWAIT    = 0x01000000;

    enum idtype_t
    {
        P_ALL,
        P_PID,
        P_PGID
    }

    int waitid(idtype_t, id_t, siginfo_t*, int);
}
else
{
    static assert(false, "Unsupported platform");
}
