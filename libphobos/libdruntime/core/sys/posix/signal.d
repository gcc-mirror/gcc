/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Sean Kelly,
              Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 * Source:    $(DRUNTIMESRC core/sys/posix/_signal.d)
 */

module core.sys.posix.signal;

import core.sys.posix.config;
public import core.stdc.signal;
public import core.sys.posix.sys.types; // for pid_t
public import core.sys.posix.time; // for timespec

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (HPPA)    version = HPPA_Any;
version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;
version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;

version (Posix):
extern (C):
//nothrow:  // this causes http://issues.dlang.org/show_bug.cgi?id=12738 (which has been fixed)
//@system:

//
// Required
//
/*
SIG_DFL (defined in core.stdc.signal)
SIG_ERR (defined in core.stdc.signal)
SIG_IGN (defined in core.stdc.signal)

sig_atomic_t (defined in core.stdc.signal)

SIGEV_NONE
SIGEV_SIGNAL
SIGEV_THREAD

union sigval
{
    int   sival_int;
    void* sival_ptr;
}

SIGRTMIN
SIGRTMAX

SIGABRT (defined in core.stdc.signal)
SIGALRM
SIGBUS
SIGCHLD
SIGCONT
SIGFPE (defined in core.stdc.signal)
SIGHUP
SIGILL (defined in core.stdc.signal)
SIGINT (defined in core.stdc.signal)
SIGKILL
SIGPIPE
SIGQUIT
SIGSEGV (defined in core.stdc.signal)
SIGSTOP
SIGTERM (defined in core.stdc.signal)
SIGTSTP
SIGTTIN
SIGTTOU
SIGUSR1
SIGUSR2
SIGURG

struct sigaction_t
{
    sigfn_t     sa_handler;
    sigset_t    sa_mask;
    sigactfn_t  sa_sigaction;
}

sigfn_t signal(int sig, sigfn_t func); (defined in core.stdc.signal)
int raise(int sig);                    (defined in core.stdc.signal)
*/

//SIG_DFL (defined in core.stdc.signal)
//SIG_ERR (defined in core.stdc.signal)
//SIG_IGN (defined in core.stdc.signal)

//sig_atomic_t (defined in core.stdc.signal)

private alias void function(int) sigfn_t;
private alias void function(int, siginfo_t*, void*) sigactfn_t;

// nothrow versions
nothrow @nogc
{
    private alias void function(int) sigfn_t2;
    private alias void function(int, siginfo_t*, void*) sigactfn_t2;
}

enum
{
  SIGEV_SIGNAL,
  SIGEV_NONE,
  SIGEV_THREAD
}

union sigval
{
    int     sival_int;
    void*   sival_ptr;
}

version (Solaris)
{
    import core.sys.posix.unistd;

    @property int SIGRTMIN() nothrow @nogc {
        __gshared static int sig = -1;
        if (sig == -1) {
            sig = cast(int)sysconf(_SC_SIGRT_MIN);
        }
        return sig;
    }

    @property int SIGRTMAX() nothrow @nogc {
        __gshared static int sig = -1;
        if (sig == -1) {
            sig = cast(int)sysconf(_SC_SIGRT_MAX);
        }
        return sig;
    }
}
else version (FreeBSD)
{
    // Note: it appears that FreeBSD (prior to 7) and OSX do not support realtime signals
    // https://github.com/freebsd/freebsd/blob/e79c62ff68fc74d88cb6f479859f6fae9baa5101/sys/sys/signal.h#L117
    enum SIGRTMIN = 65;
    enum SIGRTMAX = 126;
}
else version (DragonFlyBSD)
{
    enum SIGRTMIN = 35;
    enum SIGRTMAX = 126;
}
else version (NetBSD)
{
    enum SIGRTMIN = 33;
    enum SIGRTMAX = 63;
}
else version (linux)
{
    // Note: CRuntime_Bionic switched to calling these functions
    // since Lollipop, and Glibc, UClib and Musl all implement them
    // the same way since it's part of LSB.
    private extern (C) nothrow @nogc
    {
        int __libc_current_sigrtmin();
        int __libc_current_sigrtmax();
    }

    @property int SIGRTMIN() nothrow @nogc {
        __gshared static int sig = -1;
        if (sig == -1) {
            sig = __libc_current_sigrtmin();
        }
        return sig;
    }

    @property int SIGRTMAX() nothrow @nogc {
        __gshared static int sig = -1;
        if (sig == -1) {
            sig = __libc_current_sigrtmax();
        }
        return sig;
    }
}

version (linux)
{
    version (X86_Any)
    {
        //SIGABRT (defined in core.stdc.signal)
        enum SIGALRM    = 14;
        enum SIGBUS     = 7;
        enum SIGCHLD    = 17;
        enum SIGCONT    = 18;
        //SIGFPE (defined in core.stdc.signal)
        enum SIGHUP     = 1;
        //SIGILL (defined in core.stdc.signal)
        //SIGINT (defined in core.stdc.signal)
        enum SIGKILL    = 9;
        enum SIGPIPE    = 13;
        enum SIGQUIT    = 3;
        //SIGSEGV (defined in core.stdc.signal)
        enum SIGSTOP    = 19;
        //SIGTERM (defined in core.stdc.signal)
        enum SIGTSTP    = 20;
        enum SIGTTIN    = 21;
        enum SIGTTOU    = 22;
        enum SIGUSR1    = 10;
        enum SIGUSR2    = 12;
        enum SIGURG     = 23;
    }
    else version (HPPA_Any)
    {
        //SIGABRT (defined in core.stdc.signal)
        enum SIGALRM    = 14;
        enum SIGBUS     = 10;
        enum SIGCHLD    = 18;
        enum SIGCONT    = 26;
        //SIGFPE (defined in core.stdc.signal)
        enum SIGHUP     = 1;
        //SIGILL (defined in core.stdc.signal)
        //SIGINT (defined in core.stdc.signal)
        enum SIGKILL    = 9;
        enum SIGPIPE    = 13;
        enum SIGQUIT    = 3;
        //SIGSEGV (defined in core.stdc.signal)
        enum SIGSTOP    = 24;
        //SIGTERM (defined in core.stdc.signal)
        enum SIGTSTP    = 25;
        enum SIGTTIN    = 27;
        enum SIGTTOU    = 28;
        enum SIGUSR1    = 16;
        enum SIGUSR2    = 17;
        enum SIGURG     = 29;
    }
    else version (MIPS_Any)
    {
        //SIGABRT (defined in core.stdc.signal)
        enum SIGALRM    = 14;
        enum SIGBUS     = 10;
        enum SIGCHLD    = 18;
        enum SIGCONT    = 25;
        //SIGFPE (defined in core.stdc.signal)
        enum SIGHUP     = 1;
        //SIGILL (defined in core.stdc.signal)
        //SIGINT (defined in core.stdc.signal)
        enum SIGKILL    = 9;
        enum SIGPIPE    = 13;
        enum SIGQUIT    = 3;
        //SIGSEGV (defined in core.stdc.signal)
        enum SIGSTOP    = 23;
        //SIGTERM (defined in core.stdc.signal)
        enum SIGTSTP    = 24;
        enum SIGTTIN    = 26;
        enum SIGTTOU    = 27;
        enum SIGUSR1    = 16;
        enum SIGUSR2    = 17;
        enum SIGURG     = 21;
    }
    else version (PPC_Any)
    {
        //SIGABRT (defined in core.stdc.signal)
        enum SIGALRM    = 14;
        enum SIGBUS     = 7;
        enum SIGCHLD    = 17;
        enum SIGCONT    = 18;
        //SIGFPE (defined in core.stdc.signal)
        enum SIGHUP     = 1;
        //SIGILL (defined in core.stdc.signal)
        //SIGINT (defined in core.stdc.signal)
        enum SIGKILL    = 9;
        enum SIGPIPE    = 13;
        enum SIGQUIT    = 3;
        //SIGSEGV (defined in core.stdc.signal)
        enum SIGSTOP    = 19;
        //SIGTERM (defined in core.stdc.signal)
        enum SIGTSTP    = 20;
        enum SIGTTIN    = 21;
        enum SIGTTOU    = 22;
        enum SIGUSR1    = 10;
        enum SIGUSR2    = 12;
        enum SIGURG     = 23;
    }
    else version (ARM_Any)
    {
        //SIGABRT (defined in core.stdc.signal)
        enum SIGALRM    = 14;
        enum SIGBUS     = 7;
        enum SIGCHLD    = 17;
        enum SIGCONT    = 18;
        //SIGFPE (defined in core.stdc.signal)
        enum SIGHUP     = 1;
        //SIGILL (defined in core.stdc.signal)
        //SIGINT (defined in core.stdc.signal)
        enum SIGKILL    = 9;
        enum SIGPIPE    = 13;
        enum SIGQUIT    = 3;
        //SIGSEGV (defined in core.stdc.signal)
        enum SIGSTOP    = 19;
        //SIGTERM (defined in core.stdc.signal)
        enum SIGTSTP    = 20;
        enum SIGTTIN    = 21;
        enum SIGTTOU    = 22;
        enum SIGUSR1    = 10;
        enum SIGUSR2    = 12;
        enum SIGURG     = 23;
    }
    else version (RISCV_Any)
    {
        //SIGABRT (defined in core.stdc.signal)
        enum SIGALRM    = 14;
        enum SIGBUS     = 7;
        enum SIGCHLD    = 17;
        enum SIGCONT    = 18;
        //SIGFPE (defined in core.stdc.signal)
        enum SIGHUP     = 1;
        //SIGILL (defined in core.stdc.signal)
        //SIGINT (defined in core.stdc.signal)
        enum SIGKILL    = 9;
        enum SIGPIPE    = 13;
        enum SIGQUIT    = 3;
        //SIGSEGV (defined in core.stdc.signal)
        enum SIGSTOP    = 19;
        //SIGTERM (defined in core.stdc.signal)
        enum SIGTSTP    = 20;
        enum SIGTTIN    = 21;
        enum SIGTTOU    = 22;
        enum SIGUSR1    = 10;
        enum SIGUSR2    = 12;
        enum SIGURG     = 23;
    }
    else version (SPARC_Any)
    {
        //SIGABRT (defined in core.stdc.signal)
        enum SIGALRM    = 14;
        enum SIGBUS     = 10;
        enum SIGCHLD    = 20;
        enum SIGCONT    = 19;
        //SIGFPE (defined in core.stdc.signal)
        enum SIGHUP     = 1;
        //SIGILL (defined in core.stdc.signal)
        //SIGINT (defined in core.stdc.signal)
        enum SIGKILL    = 9;
        enum SIGPIPE    = 13;
        enum SIGQUIT    = 3;
        //SIGSEGV (defined in core.stdc.signal)
        enum SIGSTOP    = 17;
        //SIGTERM (defined in core.stdc.signal)
        enum SIGTSTP    = 18;
        enum SIGTTIN    = 21;
        enum SIGTTOU    = 22;
        enum SIGUSR1    = 30;
        enum SIGUSR2    = 31;
        enum SIGURG     = 16;
    }
    else version (IBMZ_Any)
    {
        //SIGABRT (defined in core.stdc.signal)
        enum SIGALRM    = 14;
        enum SIGBUS     = 7;
        enum SIGCHLD    = 17;
        enum SIGCONT    = 18;
        //SIGFPE (defined in core.stdc.signal)
        enum SIGHUP     = 1;
        //SIGILL (defined in core.stdc.signal)
        //SIGINT (defined in core.stdc.signal)
        enum SIGKILL    = 9;
        enum SIGPIPE    = 13;
        enum SIGQUIT    = 3;
        //SIGSEGV (defined in core.stdc.signal)
        enum SIGSTOP    = 19;
        //SIGTERM (defined in core.stdc.signal)
        enum SIGTSTP    = 20;
        enum SIGTTIN    = 21;
        enum SIGTTOU    = 22;
        enum SIGUSR1    = 10;
        enum SIGUSR2    = 12;
        enum SIGURG     = 23;
    }
    else
        static assert(0, "unimplemented");
}
else version (Darwin)
{
    //SIGABRT (defined in core.stdc.signal)
    enum SIGALRM    = 14;
    enum SIGBUS     = 10;
    enum SIGCHLD    = 20;
    enum SIGCONT    = 19;
    //SIGFPE (defined in core.stdc.signal)
    enum SIGHUP     = 1;
    //SIGILL (defined in core.stdc.signal)
    //SIGINT (defined in core.stdc.signal)
    enum SIGKILL    = 9;
    enum SIGPIPE    = 13;
    enum SIGQUIT    = 3;
    //SIGSEGV (defined in core.stdc.signal)
    enum SIGSTOP    = 17;
    //SIGTERM (defined in core.stdc.signal)
    enum SIGTSTP    = 18;
    enum SIGTTIN    = 21;
    enum SIGTTOU    = 22;
    enum SIGUSR1    = 30;
    enum SIGUSR2    = 31;
    enum SIGURG     = 16;
}
else version (FreeBSD)
{
    //SIGABRT (defined in core.stdc.signal)
    enum SIGALRM    = 14;
    enum SIGBUS     = 10;
    enum SIGCHLD    = 20;
    enum SIGCONT    = 19;
    //SIGFPE (defined in core.stdc.signal)
    enum SIGHUP     = 1;
    //SIGILL (defined in core.stdc.signal)
    //SIGINT (defined in core.stdc.signal)
    enum SIGKILL    = 9;
    enum SIGPIPE    = 13;
    enum SIGQUIT    = 3;
    //SIGSEGV (defined in core.stdc.signal)
    enum SIGSTOP    = 17;
    //SIGTERM (defined in core.stdc.signal)
    enum SIGTSTP    = 18;
    enum SIGTTIN    = 21;
    enum SIGTTOU    = 22;
    enum SIGUSR1    = 30;
    enum SIGUSR2    = 31;
    enum SIGURG     = 16;
}
else version (NetBSD)
{
    //SIGABRT (defined in core.stdc.signal)
    enum SIGALRM    = 14;
    enum SIGBUS     = 10;
    enum SIGCHLD    = 20;
    enum SIGCONT    = 19;
    //SIGFPE (defined in core.stdc.signal)
    enum SIGHUP     = 1;
    //SIGILL (defined in core.stdc.signal)
    //SIGINT (defined in core.stdc.signal)
    enum SIGKILL    = 9;
    enum SIGPIPE    = 13;
    enum SIGQUIT    = 3;
    //SIGSEGV (defined in core.stdc.signal)
    enum SIGSTOP    = 17;
    //SIGTERM (defined in core.stdc.signal)
    enum SIGTSTP    = 18;
    enum SIGTTIN    = 21;
    enum SIGTTOU    = 22;
    enum SIGUSR1    = 30;
    enum SIGUSR2    = 31;
    enum SIGURG     = 16;
}
else version (OpenBSD)
{
    //SIGABRT (defined in core.stdc.signal)
    enum SIGALRM    = 14;
    enum SIGBUS     = 10;
    enum SIGCHLD    = 20;
    enum SIGCONT    = 19;
    //SIGFPE (defined in core.stdc.signal)
    enum SIGHUP     = 1;
    //SIGILL (defined in core.stdc.signal)
    //SIGINT (defined in core.stdc.signal)
    enum SIGKILL    = 9;
    enum SIGPIPE    = 13;
    enum SIGQUIT    = 3;
    //SIGSEGV (defined in core.stdc.signal)
    enum SIGSTOP    = 17;
    //SIGTERM (defined in core.stdc.signal)
    enum SIGTSTP    = 18;
    enum SIGTTIN    = 21;
    enum SIGTTOU    = 22;
    enum SIGUSR1    = 30;
    enum SIGUSR2    = 31;
    enum SIGURG     = 16;
}
else version (DragonFlyBSD)
{
    //SIGABRT (defined in core.stdc.signal)
    enum SIGALRM    = 14;
    enum SIGBUS     = 10;
    enum SIGCHLD    = 20;
    enum SIGCONT    = 19;
    //SIGFPE (defined in core.stdc.signal)
    enum SIGHUP     = 1;
    //SIGILL (defined in core.stdc.signal)
    //SIGINT (defined in core.stdc.signal)
    enum SIGKILL    = 9;
    enum SIGPIPE    = 13;
    enum SIGQUIT    = 3;
    //SIGSEGV (defined in core.stdc.signal)
    enum SIGSTOP    = 17;
    //SIGTERM (defined in core.stdc.signal)
    enum SIGTSTP    = 18;
    enum SIGTTIN    = 21;
    enum SIGTTOU    = 22;
    enum SIGUSR1    = 30;
    enum SIGUSR2    = 31;
    enum SIGURG     = 16;
}
else version (Solaris)
{
    //SIGABRT (defined in core.stdc.signal)
    enum SIGALRM = 14;
    enum SIGBUS = 10;
    enum SIGCHLD = 18;
    enum SIGCONT = 25;
    //SIGFPE (defined in core.stdc.signal)
    enum SIGHUP = 1;
    //SIGILL (defined in core.stdc.signal)
    //SIGINT (defined in core.stdc.signal)
    enum SIGKILL = 9;
    enum SIGPIPE = 13;
    enum SIGQUIT = 3;
    //SIGSEGV (defined in core.stdc.signal)
    enum SIGSTOP = 23;
    //SIGTERM (defined in core.stdc.signal)
    enum SIGTSTP = 24;
    enum SIGTTIN = 26;
    enum SIGTTOU = 27;
    enum SIGUSR1 = 16;
    enum SIGUSR2 = 17;
    enum SIGURG = 21;
}
else
{
    static assert(false, "Unsupported platform");
}

version (linux)
{
    version (CRuntime_Musl)
    {
        struct sigaction_t
        {
            union
            {
                sigfn_t     sa_handler;
                sigactfn_t  sa_sigaction;
            }
            sigset_t        sa_mask;
            int             sa_flags;
            void function() sa_restorer;
        }
    }
    else version (CRuntime_Bionic)
    {
        version (D_LP64)
        {
            struct sigaction_t
            {
                int            sa_flags;
                union
                {
                    sigfn_t    sa_handler;
                    sigactfn_t sa_sigaction;
                }
                sigset_t        sa_mask;
                void function() sa_restorer;
        }
        }
        else
        {
            struct sigaction_t
            {
                union
                {
                    sigfn_t    sa_handler;
                    sigactfn_t sa_sigaction;
                }
                sigset_t        sa_mask;
                int             sa_flags;
                void function() sa_restorer;
            }
        }
    }
    else version (SystemZ)
    {
        struct sigaction_t
        {
            static if ( true /* __USE_POSIX199309 */ )
            {
                union
                {
                    sigfn_t     sa_handler;
                    sigactfn_t  sa_sigaction;
                }
            }
            else
            {
                sigfn_t     sa_handler;
            }
            version (CRuntime_Glibc)
            {
                int         __glibc_reserved0;
                int         sa_flags;
            }
            else
            {
                c_ulong     sa_flags;
            }

            void function() sa_restorer;

            sigset_t        sa_mask;
        }
    }
    else version (HPPA_Any)
    {
        struct sigaction_t
        {
            static if ( true /* __USE_POSIX199309 */ )
            {
                union
                {
                    sigfn_t     sa_handler;
                    sigactfn_t  sa_sigaction;
                }
            }
            else
            {
                sigfn_t     sa_handler;
            }
            version (CRuntime_Glibc)
            {
                version (D_LP64)
                    int     __glibc_reserved0;
                int         sa_flags;
            }
            else
            {
                c_ulong     sa_flags;
            }
            sigset_t        sa_mask;
        }
    }
    else version (MIPS_Any)
    {
        struct sigaction_t
        {
            int sa_flags;

            static if ( true /* __USE_POSIX199309 */ )
            {
                union
                {
                    sigfn_t     sa_handler;
                    sigactfn_t  sa_sigaction;
                }
            }
            else
            {
                sigfn_t     sa_handler;
            }
            sigset_t        sa_mask;
            void function() sa_restorer;

            version (CRuntime_Glibc)
            {
                static if ((void*).sizeof < 8)
                    int[1]  sa_resv;
            }
        }
    }
    else version (SPARC_Any)
    {
        struct sigaction_t
        {
            static if ( true /* __USE_POSIX199309 */ )
            {
                union
                {
                    sigfn_t     sa_handler;
                    sigactfn_t  sa_sigaction;
                }
            }
            else
            {
                sigfn_t     sa_handler;
            }
            version (CRuntime_Glibc)
            {
                sigset_t    sa_mask;
                version (D_LP64)
                    int     __glibc_reserved0;
                int         sa_flags;
                void function() sa_restorer;
            }
            else
            {
                c_ulong     sa_flags;
                void function() sa_restorer;
                sigset_t    sa_mask;
            }
        }
    }
    else
    {
        struct sigaction_t
        {
            static if ( true /* __USE_POSIX199309 */ )
            {
                union
                {
                    sigfn_t     sa_handler;
                    sigactfn_t  sa_sigaction;
                }
            }
            else
            {
                sigfn_t     sa_handler;
            }
            sigset_t        sa_mask;
            int             sa_flags;

            void function() sa_restorer;
        }
    }
}
else version (FreeBSD)
{
    struct sigaction_t
    {
        union
        {
            sigfn_t     sa_handler;
            sigactfn_t  sa_sigaction;
        }
        int      sa_flags;
        sigset_t sa_mask;
    }
}
else version (NetBSD)
{
    struct sigaction_t
    {
        union
        {
            sigfn_t     sa_handler;
            sigactfn_t  sa_sigaction;
        }
        sigset_t sa_mask;
        int      sa_flags;
    }
}
else version (OpenBSD)
{
    struct sigaction_t
    {
        union
        {
            sigfn_t     __sa_handler;
            alias sa_handler = __sa_handler;
            sigactfn_t  __sa_sigaction;
            alias sa_sigaction = __sa_sigaction;
        }
        sigset_t sa_mask;
        int      sa_flags;
    }
}
else version (DragonFlyBSD)
{
    struct sigaction_t
    {
        union
        {
            sigfn_t     sa_handler;
            sigactfn_t  sa_sigaction;
        }
        int      sa_flags;
        sigset_t sa_mask;
    }
}
else version (Solaris)
{
    struct sigaction_t
    {
        int sa_flags;

        union
        {
            sigfn_t sa_handler;
            sigactfn_t sa_sigaction;
        }

        sigset_t sa_mask;
        version (D_LP64) {}
        else
            int[2] sa_resv;
    }
}
else version (Darwin)
{
    struct sigaction_t
    {
        static if ( true /* __USE_POSIX199309 */ )
        {
            union
            {
                sigfn_t     sa_handler;
                sigactfn_t  sa_sigaction;
            }
        }
        else
        {
            sigfn_t     sa_handler;
        }
        sigset_t        sa_mask;
        int             sa_flags;
    }
}
else
{
    static assert(false, "Unsupported platform");
}

//
// C Extension (CX)
//
/*
SIG_HOLD

sigset_t
pid_t   (defined in core.sys.types)

SIGABRT (defined in core.stdc.signal)
SIGFPE  (defined in core.stdc.signal)
SIGILL  (defined in core.stdc.signal)
SIGINT  (defined in core.stdc.signal)
SIGSEGV (defined in core.stdc.signal)
SIGTERM (defined in core.stdc.signal)

SA_NOCLDSTOP (CX|XSI)
SIG_BLOCK
SIG_UNBLOCK
SIG_SETMASK

struct siginfo_t
{
    int     si_signo;
    int     si_code;

    version (XSI)
    {
        int     si_errno;
        pid_t   si_pid;
        uid_t   si_uid;
        void*   si_addr;
        int     si_status;
        c_long  si_band;
    }
    version (RTS)
    {
        sigval  si_value;
    }
}

SI_USER
SI_QUEUE
SI_TIMER
SI_ASYNCIO
SI_MESGQ
*/

nothrow @nogc
{

version (linux)
{
    enum SIG_HOLD = cast(sigfn_t2) 2;

    private enum _SIGSET_NWORDS = 1024 / (8 * c_ulong.sizeof);

    struct sigset_t
    {
        c_ulong[_SIGSET_NWORDS] __val;
    }

    enum SA_NOCLDSTOP   = 1; // (CX|XSI)

    version (MIPS_Any)
    {
        enum SIG_BLOCK      = 1;
        enum SIG_UNBLOCK    = 2;
        enum SIG_SETMASK    = 3;
    }
    else version (SPARC_Any)
    {
        enum SIG_BLOCK      = 1;
        enum SIG_UNBLOCK    = 2;
        enum SIG_SETMASK    = 4;
    }
    else
    {
        enum SIG_BLOCK      = 0;
        enum SIG_UNBLOCK    = 1;
        enum SIG_SETMASK    = 2;
    }

    private enum __SI_MAX_SIZE = 128;

    static if ( __WORDSIZE == 64 )
    {
        private enum __SI_PAD_SIZE = ((__SI_MAX_SIZE / int.sizeof) - 4);
    }
    else
    {
        private enum __SI_PAD_SIZE = ((__SI_MAX_SIZE / int.sizeof) - 3);
    }

    struct siginfo_t
    {
        int si_signo;
        version (MIPS_Any)  // __SI_SWAP_ERRNO_CODE
        {
            int si_code;
            int si_errno;
        }
        else
        {
            int si_errno;
            int si_code;
        }

        union _sifields_t
        {
            int[__SI_PAD_SIZE] _pad;

            // kill()
            struct _kill_t
            {
                pid_t si_pid;
                uid_t si_uid;
            } _kill_t _kill;
            // POSIX.1b timers.
            struct _timer_t
            {
                int    si_tid;
                int    si_overrun;
                sigval si_sigval;
            } _timer_t _timer;

            // POSIX.1b signals
            struct _rt_t
            {
                pid_t  si_pid;
                uid_t  si_uid;
                sigval si_sigval;
            } _rt_t _rt;

            // SIGCHLD
            struct _sigchild_t
            {
                pid_t   si_pid;
                uid_t   si_uid;
                int     si_status;
                clock_t si_utime;
                clock_t si_stime;
            } _sigchild_t _sigchld;

            // SIGILL, SIGFPE, SIGSEGV, SIGBUS
            struct _sigfault_t
            {
                void*     si_addr;
            } _sigfault_t _sigfault;

            // SIGPOLL
            struct _sigpoll_t
            {
                c_long   si_band;
                int      si_fd;
            } _sigpoll_t _sigpoll;
        } _sifields_t _sifields;

    nothrow @nogc:
        @property ref pid_t si_pid() return { return _sifields._kill.si_pid; }
        @property ref uid_t si_uid() return { return _sifields._kill.si_uid; }
        @property ref void* si_addr() return { return _sifields._sigfault.si_addr; }
        @property ref int si_status() return { return _sifields._sigchld.si_status; }
        @property ref c_long si_band() return { return _sifields._sigpoll.si_band; }
        @property ref sigval si_value() return { return _sifields._rt.si_sigval; }
    }

    enum
    {
        SI_ASYNCNL = -60,
        SI_TKILL   = -6,
        SI_SIGIO,
        SI_ASYNCIO,
        SI_MESGQ,
        SI_TIMER,
        SI_QUEUE,
        SI_USER,
        SI_KERNEL  = 0x80
    }
}
else version (Darwin)
{
    enum SIG_HOLD = cast(sigfn_t2) 5;

    alias uint sigset_t;

    enum SA_NOCLDSTOP = 8; // (CX|XSI)

    enum SIG_BLOCK   = 1;
    enum SIG_UNBLOCK = 2;
    enum SIG_SETMASK = 3;

    struct siginfo_t
    {
        int     si_signo;
        int     si_errno;
        int     si_code;
        pid_t   si_pid;
        uid_t   si_uid;
        int     si_status;
        void*   si_addr;
        sigval  si_value;
        int     si_band;
        uint[7] pad;
    }

    enum SI_USER    = 0x10001;
    enum SI_QUEUE   = 0x10002;
    enum SI_TIMER   = 0x10003;
    enum SI_ASYNCIO = 0x10004;
    enum SI_MESGQ   = 0x10005;
}
else version (FreeBSD)
{
    enum SIG_HOLD = cast(sigfn_t2) 3;

    struct sigset_t
    {
        uint[4] __bits;
    }

    enum SA_NOCLDSTOP = 8;

    enum SIG_BLOCK = 1;
    enum SIG_UNBLOCK = 2;
    enum SIG_SETMASK = 3;

    struct siginfo_t
    {
        int si_signo;
        int si_errno;
        int si_code;
        pid_t si_pid;
        uid_t si_uid;
        int si_status;
        void* si_addr;
        sigval si_value;
        union __reason
        {
            struct __fault
            {
                int _trapno;
            }
            __fault _fault;
            struct __timer
            {
                int _timerid;
                int _overrun;
            }
            __timer _timer;
            struct __mesgq
            {
                int _mqd;
            }
            __mesgq _mesgq;
            struct __poll
            {
                c_long _band;
            }
            __poll _poll;
            struct ___spare___
            {
                c_long __spare1__;
                int[7] __spare2__;
            }
            ___spare___ __spare__;
        }
        __reason _reason;

        @property ref c_long si_band() return { return _reason._poll._band; }
    }

    enum SI_USER    = 0x10001;
    enum SI_QUEUE   = 0x10002;
    enum SI_TIMER   = 0x10003;
    enum SI_ASYNCIO = 0x10004;
    enum SI_MESGQ   = 0x10005;
}
else version (NetBSD)
{
    enum SIG_HOLD = cast(sigfn_t2) 3;

    struct sigset_t
    {
        uint[4] __bits;
    }

    enum SA_NOCLDSTOP = 8;

    enum SIG_BLOCK = 1;
    enum SIG_UNBLOCK = 2;
    enum SIG_SETMASK = 3;

    union sigval_t
    {
        int   sival_int;
        void* sival_ptr;
    }

    struct _ksiginfo
    {
        int     _signo;
        int     _code;
        int     _errno;
        version (D_LP64)
            int _pad;

        union reason_t
        {
            struct rt_t
            {
                pid_t    _pid;
                uid_t    _uid;
                sigval_t _value;
            } rt_t _rt;
            struct child_t
            {
                pid_t   _pid;
                uid_t   _uid;
                int     _status;
                clock_t _utime;
                clock_t _stime;
            } child_t _child;
            struct fault_t
            {
                void* _addr;
                int   _trap;
                int   _trap2;
                int   _trap3;
            } fault_t fault;
            struct poll_t
            {
                c_long _band;
                int  _fd;
            } poll_t _poll;
        }
        reason_t _reason;
    }

    union siginfo_t
    {
        ubyte[128] si_pad;
        _ksiginfo _info;
        @property ref c_long si_band() return { return _info._reason._poll._band; }
    }

    enum SI_USER    = 0;
    enum SI_QUEUE   = -1;
    enum SI_TIMER   = -2;
    enum SI_ASYNCIO = -3;
    enum SI_MESGQ   = -4;
}
else version (OpenBSD)
{
    enum SIG_CATCH = cast(sigfn_t2) 2;
    enum SIG_HOLD = cast(sigfn_t2) 3;

    alias sigset_t = uint;

    enum SA_NOCLDSTOP = 0x0008;

    enum SIG_BLOCK = 1;
    enum SIG_UNBLOCK = 2;
    enum SIG_SETMASK = 3;

    private enum SI_MAXSZ = 128;
    private enum SI_PAD = (SI_MAXSZ / int.sizeof) - 3;

    struct siginfo_t
    {
        int si_signo;
        int si_errno;
        int si_code;
        union _data
        {
            int[SI_PAD] _pad;
            struct _proc
            {
                pid_t _pid;
                union _pdata
                {
                    struct _kill
                    {
                        uid_t _uid;
                        sigval _value;
                    }
                    struct _cld
                    {
                        clock_t _utime;
                        clock_t _stime;
                        int _status;
                    }
                }
            }
            struct _fault
            {
                caddr_t _addr;
                int _trapno;
            }
        }
        alias si_pid     = _data._proc._pid;
        alias si_status  = _data._proc._pdata._cld._status;
        alias si_stime   = _data._proc._pdata._cld._stime;
        alias si_utime   = _data._proc._pdata._cld._utime;
        alias si_uid     = _data._proc._pdata._kill._uid;
        alias si_value   = _data._proc._pdata._kill._value;
        alias si_addr    = _data._fault._addr;
        alias si_trapno  = _data._fault._trapno;
    }

    enum SI_NOINFO = 32767;
    enum SI_USER   = 0;
    enum SI_LWP    = -1;
    enum SI_QUEUE  = -2;
    enum SI_TIMER  = -3;
}
else version (DragonFlyBSD)
{
    enum SIG_CATCH = cast(sigfn_t2) 2;
    enum SIG_HOLD = cast(sigfn_t2) 3;

    struct sigset_t
    {
        uint[4] __bits;
    }

    enum SA_NOCLDSTOP = 8;

    enum SIG_BLOCK = 1;
    enum SIG_UNBLOCK = 2;
    enum SIG_SETMASK = 3;

    struct siginfo_t
    {
        int si_signo;
        int si_errno;
        int si_code;
        int si_pid;
        uint si_uid;
        int si_status;
        void* si_addr;
        sigval si_value;
        c_long si_band;
        int[7]   __spare;
    }

    enum SI_UNDEFINED = 0x00000;
    enum SI_USER      =  0;
    enum SI_QUEUE     = -1;
    enum SI_TIMER     = -2;
    enum SI_ASYNCIO   = -3;
    enum SI_MESGQ     = -4;
}
else version (Solaris)
{
    enum SIG_HOLD = cast(sigfn_t2)2;

    struct sigset_t
    {
        uint[4] __bits;
    }

    enum SIG_BLOCK = 1;
    enum SIG_UNBLOCK = 2;
    enum SIG_SETMASK = 3;

    struct siginfo_t
    {
        int si_signo;
        int si_code;
        int si_errno;

        version (D_LP64)
            int si_pad;

        union ___data
        {
            version (D_LP64)
                int[(256 / int.sizeof) - 4] si_pad;
            else
                int[(128 / int.sizeof) - 3] si_pad;

            struct ___proc
            {
                pid_t __pid;

                union ___pdata
                {
                    struct ___kill
                    {
                        uid_t __uid;
                        sigval __value;
                    }

                    ___kill __kill;

                    struct ___cld
                    {
                        clock_t __utime;
                        int __status;
                        clock_t __stime;
                    }

                    ___cld __cld;
                }

                ___pdata __pdata;
                ctid_t __ctid;
                zoneid_t __zoneid;
            }

            ___proc __proc;

            struct ___fault
            {
                void* __addr;
                int __trapno;
                caddr_t __pc;
            }

            ___fault __fault;

            struct ___file
            {
                int __fd;
                c_long __band;
            }

            ___file __file;

            struct ___prof
            {
                caddr_t __faddr;
                timestruc_t __tstamp;
                short __syscall;
                char __nsysarg = 0;
                char __fault = 0;
                c_long[8] __sysarg;
                int[10] __mstate;
            }

            ___prof __prof;

            struct ___rctl
            {
                int __entity;
            }

            ___rctl __rctl;
        }

        ___data __data;
    }

    enum SI_NOINFO  = 32767;
    enum SI_DTRACE  = 2050;
    enum SI_RCTL    = 2049;
    enum SI_USER    = 0;
    enum SI_LWP     = -1;
    enum SI_QUEUE   = -2;
    enum SI_TIMER   = -3;
    enum SI_ASYNCIO = -4;
    enum SI_MESGQ   = -5;
}
else
{
    static assert(false, "Unsupported platform");
}

/*
int kill(pid_t, int);
int sigaction(int, const scope sigaction_t*, sigaction_t*);
int sigaddset(sigset_t*, int);
int sigdelset(sigset_t*, int);
int sigemptyset(sigset_t*);
int sigfillset(sigset_t*);
int sigismember(const scope sigset_t*, int);
int sigpending(sigset_t*);
int sigprocmask(int, const scope sigset_t*, sigset_t*);
int sigsuspend(const scope sigset_t*);
int sigwait(const scope sigset_t*, int*);
*/

version (CRuntime_Glibc)
{
    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);
    int sigaddset(sigset_t*, int);
    int sigdelset(sigset_t*, int);
    int sigemptyset(sigset_t*);
    int sigfillset(sigset_t*);
    int sigismember(const scope sigset_t*, int);
    int sigpending(sigset_t*);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else version (Darwin)
{
    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);
    int sigaddset(sigset_t*, int);
    int sigdelset(sigset_t*, int);
    int sigemptyset(sigset_t*);
    int sigfillset(sigset_t*);
    int sigismember(const scope sigset_t*, int);
    int sigpending(sigset_t*);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else version (FreeBSD)
{
    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);
    int sigaddset(sigset_t*, int);
    int sigdelset(sigset_t*, int);
    int sigemptyset(sigset_t *);
    int sigfillset(sigset_t *);
    int sigismember(const scope sigset_t*, int);
    int sigpending(sigset_t *);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else version (NetBSD)
{
    int kill(pid_t, int);
    int __sigaction14(int, const scope sigaction_t*, sigaction_t*);
    int __sigaddset14(sigset_t*, int);
    int __sigdelset14(sigset_t*, int);
    int __sigemptyset14(sigset_t *);
    int __sigfillset14(sigset_t *);
    int __sigismember14(const scope sigset_t*, int);
    int __sigpending14(sigset_t *);
    int __sigprocmask14(int, const scope sigset_t*, sigset_t*);
    int __sigsuspend14(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);

    alias __sigaction14 sigaction;
    alias __sigaddset14 sigaddset;
    alias __sigdelset14 sigdelset;
    alias __sigemptyset14 sigemptyset;
    alias __sigfillset14 sigfillset;
    alias __sigismember14 sigismember;
    alias __sigpending14 sigpending;
    alias __sigprocmask14 sigprocmask;
    alias __sigsuspend14 sigsuspend;
}
else version (OpenBSD)
{
    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);
    int sigaddset(sigset_t*, int);
    int sigdelset(sigset_t*, int);
    int sigemptyset(sigset_t *);
    int sigfillset(sigset_t *);
    int sigismember(const scope sigset_t*, int);
    int sigpending(sigset_t *);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else version (DragonFlyBSD)
{
    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);
    int sigaddset(sigset_t*, int);
    int sigdelset(sigset_t*, int);
    int sigemptyset(sigset_t *);
    int sigfillset(sigset_t *);
    int sigismember(const scope sigset_t*, int);
    int sigpending(sigset_t *);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else version (Solaris)
{
    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);
    int sigaddset(sigset_t*, int);
    int sigdelset(sigset_t*, int);
    int sigemptyset(sigset_t*);
    int sigfillset(sigset_t*);
    int sigismember(const scope sigset_t*, int);
    int sigpending(sigset_t*);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else version (CRuntime_Bionic)
{
    public import core.sys.posix.time: timer_t;
    import core.stdc.string : memset;

    version (X86)
        enum int LONG_BIT = 32;
    else version (ARM)
        enum int LONG_BIT = 32;
    else version (AArch64)
        enum int LONG_BIT = 64;
    else version (X86_64)
        enum int LONG_BIT = 64;
    else
        static assert(false, "Architecture not supported.");

    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);

    // These functions are defined inline in bionic.
    int sigaddset(sigset_t* set, int signum)
    {
        c_ulong* local_set = cast(c_ulong*) set;
        signum--;
        local_set[signum/LONG_BIT] |= 1UL << (signum%LONG_BIT);
        return 0;
    }

    int sigdelset(sigset_t* set, int signum)
    {
        c_ulong* local_set = cast(c_ulong*) set;
        signum--;
        local_set[signum/LONG_BIT] &= ~(1UL << (signum%LONG_BIT));
        return 0;
    }

    int sigemptyset(sigset_t* set) { memset(set, 0, (*set).sizeof); return 0; }

    int sigfillset(sigset_t* set) { memset(set, ~0, (*set).sizeof); return 0; }

    int sigismember(sigset_t* set, int signum)
    {
        c_ulong* local_set = cast(c_ulong*) set;
        signum--;
        return cast(int) ((local_set[signum/LONG_BIT] >> (signum%LONG_BIT)) & 1);
    }

    int sigpending(sigset_t*);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else version (CRuntime_Musl)
{
    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);
    int sigaddset(sigset_t*, int);
    int sigdelset(sigset_t*, int);
    int sigemptyset(sigset_t*);
    int sigfillset(sigset_t*);
    int sigismember(const scope sigset_t*, int);
    int sigpending(sigset_t*);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else version (CRuntime_UClibc)
{
    int kill(pid_t, int);
    int sigaction(int, const scope sigaction_t*, sigaction_t*);
    int sigaddset(sigset_t*, int);
    int sigdelset(sigset_t*, int);
    int sigemptyset(sigset_t*);
    int sigfillset(sigset_t*);
    int sigismember(const scope sigset_t*, int);
    int sigpending(sigset_t*);
    int sigprocmask(int, const scope sigset_t*, sigset_t*);
    int sigsuspend(const scope sigset_t*);
    int sigwait(const scope sigset_t*, int*);
}
else
{
    static assert(false, "Unsupported platform");
}
}

//
// XOpen (XSI)
//
/*
SIGPOLL
SIGPROF
SIGSYS
SIGTRAP
SIGVTALRM
SIGXCPU
SIGXFSZ

SA_ONSTACK
SA_RESETHAND
SA_RESTART
SA_SIGINFO
SA_NOCLDWAIT
SA_NODEFER

ILL_ILLOPC
ILL_ILLOPN
ILL_ILLADR
ILL_ILLTRP
ILL_PRVOPC
ILL_PRVREG
ILL_COPROC
ILL_BADSTK

FPE_INTDIV
FPE_INTOVF
FPE_FLTDIV
FPE_FLTOVF
FPE_FLTUND
FPE_FLTRES
FPE_FLTINV
FPE_FLTSUB

SEGV_MAPERR
SEGV_ACCERR

BUS_ADRALN
BUS_ADRERR
BUS_OBJERR

TRAP_BRKPT
TRAP_TRACE

CLD_EXITED
CLD_KILLED
CLD_DUMPED
CLD_TRAPPED
CLD_STOPPED
CLD_CONTINUED

POLL_IN
POLL_OUT
POLL_MSG
POLL_ERR
POLL_PRI
POLL_HUP
*/

version (linux)
{
    version (X86_Any)
    {
        enum SIGPOLL        = 29;
        enum SIGPROF        = 27;
        enum SIGSYS         = 31;
        enum SIGTRAP        = 5;
        enum SIGVTALRM      = 26;
        enum SIGXCPU        = 24;
        enum SIGXFSZ        = 25;
    }
    else version (HPPA_Any)
    {
        enum SIGPOLL    = 22;
        enum SIGPROF    = 21;
        enum SIGSYS     = 31;
        enum SIGTRAP    = 5;
        enum SIGVTALRM  = 20;
        enum SIGXCPU    = 12;
        enum SIGXFSZ    = 30;
    }
    else version (MIPS_Any)
    {
        enum SIGPOLL    = 22;
        enum SIGPROF    = 29;
        enum SIGSYS     = 12;
        enum SIGTRAP    = 5;
        enum SIGVTALRM  = 28;
        enum SIGXCPU    = 30;
        enum SIGXFSZ    = 31;
    }
    else version (PPC_Any)
    {
        enum SIGPOLL    = 29;
        enum SIGPROF    = 27;
        enum SIGSYS     = 31;
        enum SIGTRAP    = 5;
        enum SIGVTALRM  = 26;
        enum SIGXCPU    = 24;
        enum SIGXFSZ    = 25;
    }
    else version (ARM_Any)
    {
        enum SIGPOLL    = 29;
        enum SIGPROF    = 27;
        enum SIGSYS     = 31;
        enum SIGTRAP    = 5;
        enum SIGVTALRM  = 26;
        enum SIGXCPU    = 24;
        enum SIGXFSZ    = 25;
    }
    else version (RISCV_Any)
    {
        enum SIGPOLL    = 29;
        enum SIGPROF    = 27;
        enum SIGSYS     = 31;
        enum SIGTRAP    = 5;
        enum SIGVTALRM  = 26;
        enum SIGXCPU    = 24;
        enum SIGXFSZ    = 25;
    }
    else version (SPARC_Any)
    {
        enum SIGPOLL    = 23;
        enum SIGPROF    = 27;
        enum SIGSYS     = 12;
        enum SIGTRAP    = 5;
        enum SIGVTALRM  = 26;
        enum SIGXCPU    = 24;
        enum SIGXFSZ    = 25;
    }
    else version (IBMZ_Any)
    {
        enum SIGPOLL    = 29;
        enum SIGPROF    = 27;
        enum SIGSYS     = 31;
        enum SIGTRAP    = 5;
        enum SIGVTALRM  = 26;
        enum SIGXCPU    = 24;
        enum SIGXFSZ    = 25;
    }
    else
        static assert(0, "unimplemented");

    version (MIPS_Any)
    {
        enum SA_ONSTACK   = 0x08000000;
        enum SA_RESETHAND = 0x80000000;
        enum SA_RESTART   = 0x10000000;
        enum SA_SIGINFO   = 8;
        enum SA_NOCLDWAIT = 0x10000;
        enum SA_NODEFER   = 0x40000000;
    }
    else
    {
        enum SA_ONSTACK   = 0x08000000;
        enum SA_RESETHAND = 0x80000000;
        enum SA_RESTART   = 0x10000000;
        enum SA_SIGINFO   = 4;
        enum SA_NOCLDWAIT = 2;
        enum SA_NODEFER   = 0x40000000;
    }

    enum SA_NOMASK      = SA_NODEFER;
    enum SA_ONESHOT     = SA_RESETHAND;
    enum SA_STACK       = SA_ONSTACK;

    enum
    {
        ILL_ILLOPC = 1,
        ILL_ILLOPN,
        ILL_ILLADR,
        ILL_ILLTRP,
        ILL_PRVOPC,
        ILL_PRVREG,
        ILL_COPROC,
        ILL_BADSTK
    }

    enum
    {
        FPE_INTDIV = 1,
        FPE_INTOVF,
        FPE_FLTDIV,
        FPE_FLTOVF,
        FPE_FLTUND,
        FPE_FLTRES,
        FPE_FLTINV,
        FPE_FLTSUB
    }

    enum
    {
        SEGV_MAPERR = 1,
        SEGV_ACCERR
    }

    enum
    {
        BUS_ADRALN = 1,
        BUS_ADRERR,
        BUS_OBJERR
    }

    enum
    {
        TRAP_BRKPT = 1,
        TRAP_TRACE
    }

    enum
    {
        CLD_EXITED = 1,
        CLD_KILLED,
        CLD_DUMPED,
        CLD_TRAPPED,
        CLD_STOPPED,
        CLD_CONTINUED
    }

    enum
    {
        POLL_IN = 1,
        POLL_OUT,
        POLL_MSG,
        POLL_ERR,
        POLL_PRI,
        POLL_HUP
    }
}
else version (Darwin)
{
    enum SIGPOLL        = 7;
    enum SIGPROF        = 27;
    enum SIGSYS         = 12;
    enum SIGTRAP        = 5;
    enum SIGVTALRM      = 26;
    enum SIGXCPU        = 24;
    enum SIGXFSZ        = 25;

    enum SA_ONSTACK     = 0x0001;
    enum SA_RESETHAND   = 0x0004;
    enum SA_RESTART     = 0x0002;
    enum SA_SIGINFO     = 0x0040;
    enum SA_NOCLDWAIT   = 0x0020;
    enum SA_NODEFER     = 0x0010;

    enum ILL_ILLOPC = 1;
    enum ILL_ILLOPN = 4;
    enum ILL_ILLADR = 5;
    enum ILL_ILLTRP = 2;
    enum ILL_PRVOPC = 3;
    enum ILL_PRVREG = 6;
    enum ILL_COPROC = 7;
    enum ILL_BADSTK = 8;

    enum FPE_INTDIV = 7;
    enum FPE_INTOVF = 8;
    enum FPE_FLTDIV = 1;
    enum FPE_FLTOVF = 2;
    enum FPE_FLTUND = 3;
    enum FPE_FLTRES = 4;
    enum FPE_FLTINV = 5;
    enum FPE_FLTSUB = 6;

    enum
    {
        SEGV_MAPERR = 1,
        SEGV_ACCERR
    }

    enum
    {
        BUS_ADRALN = 1,
        BUS_ADRERR,
        BUS_OBJERR
    }

    enum
    {
        TRAP_BRKPT = 1,
        TRAP_TRACE
    }

    enum
    {
        CLD_EXITED = 1,
        CLD_KILLED,
        CLD_DUMPED,
        CLD_TRAPPED,
        CLD_STOPPED,
        CLD_CONTINUED
    }

    enum
    {
        POLL_IN = 1,
        POLL_OUT,
        POLL_MSG,
        POLL_ERR,
        POLL_PRI,
        POLL_HUP
    }
}
else version (FreeBSD)
{
    // No SIGPOLL on *BSD
    enum SIGPROF        = 27;
    enum SIGSYS         = 12;
    enum SIGTRAP        = 5;
    enum SIGVTALRM      = 26;
    enum SIGXCPU        = 24;
    enum SIGXFSZ        = 25;

    enum
    {
        SA_ONSTACK      = 0x0001,
        SA_RESTART      = 0x0002,
        SA_RESETHAND    = 0x0004,
        SA_NODEFER      = 0x0010,
        SA_NOCLDWAIT    = 0x0020,
        SA_SIGINFO      = 0x0040,
    }

    enum
    {
        ILL_ILLOPC = 1,
        ILL_ILLOPN,
        ILL_ILLADR,
        ILL_ILLTRP,
        ILL_PRVOPC,
        ILL_PRVREG,
        ILL_COPROC,
        ILL_BADSTK,
    }

    enum
    {
        BUS_ADRALN = 1,
        BUS_ADRERR,
        BUS_OBJERR,
    }

    enum
    {
        SEGV_MAPERR = 1,
        SEGV_ACCERR,
    }

    enum
    {
        FPE_INTOVF = 1,
        FPE_INTDIV,
        FPE_FLTDIV,
        FPE_FLTOVF,
        FPE_FLTUND,
        FPE_FLTRES,
        FPE_FLTINV,
        FPE_FLTSUB,
    }

    enum
    {
        TRAP_BRKPT = 1,
        TRAP_TRACE,
    }

    enum
    {
        CLD_EXITED = 1,
        CLD_KILLED,
        CLD_DUMPED,
        CLD_TRAPPED,
        CLD_STOPPED,
        CLD_CONTINUED,
    }

    enum
    {
        POLL_IN = 1,
        POLL_OUT,
        POLL_MSG,
        POLL_ERR,
        POLL_PRI,
        POLL_HUP,
    }
}
else version (NetBSD)
{
    // No SIGPOLL on *BSD
    enum SIGPROF        = 27;
    enum SIGSYS         = 12;
    enum SIGTRAP        = 5;
    enum SIGVTALRM      = 26;
    enum SIGXCPU        = 24;
    enum SIGXFSZ        = 25;

    enum
    {
        SA_ONSTACK      = 0x0001,
        SA_RESTART      = 0x0002,
        SA_RESETHAND    = 0x0004,
        SA_NODEFER      = 0x0010,
        SA_NOCLDWAIT    = 0x0020,
        SA_SIGINFO      = 0x0040,
    }

    enum
    {
        ILL_ILLOPC = 1,
        ILL_ILLOPN,
        ILL_ILLADR,
        ILL_ILLTRP,
        ILL_PRVOPC,
        ILL_PRVREG,
        ILL_COPROC,
        ILL_BADSTK,
    }

    enum
    {
        BUS_ADRALN = 1,
        BUS_ADRERR,
        BUS_OBJERR,
    }

    enum
    {
        SEGV_MAPERR = 1,
        SEGV_ACCERR,
    }

    enum
    {
        FPE_INTOVF = 1,
        FPE_INTDIV,
        FPE_FLTDIV,
        FPE_FLTOVF,
        FPE_FLTUND,
        FPE_FLTRES,
        FPE_FLTINV,
        FPE_FLTSUB,
    }

    enum
    {
        TRAP_BRKPT = 1,
        TRAP_TRACE,
    }

    enum
    {
        CLD_EXITED = 1,
        CLD_KILLED,
        CLD_DUMPED,
        CLD_TRAPPED,
        CLD_STOPPED,
        CLD_CONTINUED,
    }

    enum
    {
        POLL_IN = 1,
        POLL_OUT,
        POLL_MSG,
        POLL_ERR,
        POLL_PRI,
        POLL_HUP,
    }
}
else version (OpenBSD)
{
    // No SIGPOLL on *BSD
    enum SIGPROF        = 27;
    enum SIGSYS         = 12;
    enum SIGTRAP        = 5;
    enum SIGVTALRM      = 26;
    enum SIGXCPU        = 24;
    enum SIGXFSZ        = 25;

    enum
    {
        SA_ONSTACK      = 0x0001,
        SA_RESTART      = 0x0002,
        SA_RESETHAND    = 0x0004,
        SA_NODEFER      = 0x0010,
        SA_NOCLDWAIT    = 0x0020,
        SA_SIGINFO      = 0x0040,
    }

    enum
    {
        ILL_ILLOPC = 1,
        ILL_ILLOPN,
        ILL_ILLADR,
        ILL_ILLTRP,
        ILL_PRVOPC,
        ILL_PRVREG,
        ILL_COPROC,
        ILL_BADSTK,
        NSIGILL = ILL_BADSTK,
    }

    enum
    {
        BUS_ADRALN = 1,
        BUS_ADRERR,
        BUS_OBJERR,
        NSIGBUS = BUS_OBJERR,
    }

    enum
    {
        SEGV_MAPERR = 1,
        SEGV_ACCERR,
        NSIGSEGV = SEGV_ACCERR,
    }

    enum
    {
        FPE_INTDIV = 1,
        FPE_INTOVF,
        FPE_FLTDIV,
        FPE_FLTOVF,
        FPE_FLTUND,
        FPE_FLTRES,
        FPE_FLTINV,
        FPE_FLTSUB,
        NSIGFPE = FPE_FLTSUB,
    }

    enum
    {
        TRAP_BRKPT = 1,
        TRAP_TRACE,
        NSIGTRAP = TRAP_TRACE,
    }

    enum
    {
        CLD_EXITED = 1,
        CLD_KILLED,
        CLD_DUMPED,
        CLD_TRAPPED,
        CLD_STOPPED,
        CLD_CONTINUED,
        NSIGCLD = CLD_CONTINUED,
    }

    enum
    {
        POLL_IN = 1,
        POLL_OUT,
        POLL_MSG,
        POLL_ERR,
        POLL_PRI,
        POLL_HUP,
        NSIGPOLL = POLL_HUP,
    }
}
else version (DragonFlyBSD)
{
    // No SIGPOLL on *BSD
    enum SIGPROF        = 27;
    enum SIGSYS         = 12;
    enum SIGTRAP        = 5;
    enum SIGVTALRM      = 26;
    enum SIGXCPU        = 24;
    enum SIGXFSZ        = 25;

    enum
    {
        SA_ONSTACK      = 0x0001,
        SA_RESTART      = 0x0002,
        SA_RESETHAND    = 0x0004,
        SA_NODEFER      = 0x0010,
        SA_NOCLDWAIT    = 0x0020,
        SA_SIGINFO      = 0x0040,
    }

    enum
    {
        ILL_ILLOPC = 1,
        ILL_ILLOPN,
        ILL_ILLADR,
        ILL_ILLTRP,
        ILL_PRVOPC,
        ILL_PRVREG,
        ILL_COPROC,
        ILL_BADSTK,
    }

    enum
    {
        BUS_ADRALN = 1,
        BUS_ADRERR,
        BUS_OBJERR,
    }

    enum
    {
        SEGV_MAPERR = 1,
        SEGV_ACCERR,
    }

    enum
    {
        FPE_INTOVF = 1,
        FPE_INTDIV,
        FPE_FLTDIV,
        FPE_FLTOVF,
        FPE_FLTUND,
        FPE_FLTRES,
        FPE_FLTINV,
        FPE_FLTSUB,
    }

    enum
    {
        TRAP_BRKPT = 1,
        TRAP_TRACE,
    }

    enum
    {
        CLD_EXITED = 1,
        CLD_KILLED,
        CLD_DUMPED,
        CLD_TRAPPED,
        CLD_STOPPED,
        CLD_CONTINUED,
    }

    enum
    {
        POLL_IN = 1,
        POLL_OUT,
        POLL_MSG,
        POLL_ERR,
        POLL_PRI,
        POLL_HUP,
    }
}
else version (Solaris)
{
    enum SIGPOLL = 22;
    enum SIGIO = SIGPOLL;
    enum SIGPROF = 29;
    enum SIGSYS = 12;
    enum SIGTRAP = 5;
    enum SIGVTALRM = 28;
    enum SIGXCPU = 30;
    enum SIGXFSZ = 31;

    enum
    {
        SA_ONSTACK = 0x00001,
        SA_RESTART = 0x00004,
        SA_RESETHAND = 0x00002,
        SA_NODEFER = 0x00010,
        SA_NOCLDWAIT = 0x10000,
        SA_SIGINFO = 0x00008,
    }

    enum
    {
        ILL_ILLOPC = 1,
        ILL_ILLOPN,
        ILL_ILLADR,
        ILL_ILLTRP,
        ILL_PRVOPC,
        ILL_PRVREG,
        ILL_COPROC,
        ILL_BADSTK,
    }

    enum
    {
        BUS_ADRALN = 1,
        BUS_ADRERR,
        BUS_OBJERR,
    }

    enum
    {
        SEGV_MAPERR = 1,
        SEGV_ACCERR,
    }

    enum
    {
        FPE_INTDIV = 1,
        FPE_INTOVF,
        FPE_FLTDIV,
        FPE_FLTOVF,
        FPE_FLTUND,
        FPE_FLTRES,
        FPE_FLTINV,
        FPE_FLTSUB,
        FPE_FLTDEN,
    }

    enum
    {
        TRAP_BRKPT = 1,
        TRAP_TRACE,
        TRAP_RWATCH,
        TRAP_WWATCH,
        TRAP_XWATCH,
        TRAP_DTRACE,
    }

    enum
    {
        CLD_EXITED = 1,
        CLD_KILLED,
        CLD_DUMPED,
        CLD_TRAPPED,
        CLD_STOPPED,
        CLD_CONTINUED,
    }

    enum
    {
        POLL_IN = 1,
        POLL_OUT,
        POLL_MSG,
        POLL_ERR,
        POLL_PRI,
        POLL_HUP,
    }
}
else
{
    static assert(false, "Unsupported platform");
}

/*
SS_ONSTACK
SS_DISABLE
MINSIGSTKSZ
SIGSTKSZ

ucontext_t // from ucontext
mcontext_t // from ucontext

struct stack_t
{
    void*   ss_sp;
    size_t  ss_size;
    int     ss_flags;
}

struct sigstack
{
    int   ss_onstack;
    void* ss_sp;
}

sigfn_t bsd_signal(int sig, sigfn_t func);
sigfn_t sigset(int sig, sigfn_t func);

int killpg(pid_t, int);
int sigaltstack(const scope stack_t*, stack_t*);
int sighold(int);
int sigignore(int);
int siginterrupt(int, int);
int sigpause(int);
int sigrelse(int);
*/

version (CRuntime_Glibc)
{
    enum SS_ONSTACK     = 1;
    enum SS_DISABLE     = 2;
    enum MINSIGSTKSZ    = 2048;
    enum SIGSTKSZ       = 8192;

    //ucontext_t (defined in core.sys.posix.ucontext)
    //mcontext_t (defined in core.sys.posix.ucontext)

    struct stack_t
    {
        void*   ss_sp;
        int     ss_flags;
        size_t  ss_size;
    }

    struct sigstack
    {
        void*   ss_sp;
        int     ss_onstack;
    }

    sigfn_t bsd_signal(int sig, sigfn_t func);
    sigfn_t sigset(int sig, sigfn_t func);

  nothrow:
  @nogc:
    sigfn_t2 bsd_signal(int sig, sigfn_t2 func);
    sigfn_t2 sigset(int sig, sigfn_t2 func);

    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int sighold(int);
    int sigignore(int);
    int siginterrupt(int, int);
    int sigpause(int);
    int sigrelse(int);
}
else version (Darwin)
{
    enum SS_ONSTACK     = 0x0001;
    enum SS_DISABLE     = 0x0004;
    enum MINSIGSTKSZ    = 32768;
    enum SIGSTKSZ       = 131072;

    //ucontext_t (defined in core.sys.posix.ucontext)
    //mcontext_t (defined in core.sys.posix.ucontext)

    struct stack_t
    {
        void*   ss_sp;
        size_t  ss_size;
        int     ss_flags;
    }

    struct sigstack
    {
        void*   ss_sp;
        int     ss_onstack;
    }

    sigfn_t bsd_signal(int sig, sigfn_t func);
    sigfn_t sigset(int sig, sigfn_t func);

  nothrow:
  @nogc:
    sigfn_t2 bsd_signal(int sig, sigfn_t2 func);
    sigfn_t2 sigset(int sig, sigfn_t2 func);

    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int sighold(int);
    int sigignore(int);
    int siginterrupt(int, int);
    int sigpause(int);
    int sigrelse(int);
}
else version (FreeBSD)
{
    enum
    {
        SS_ONSTACK = 0x0001,
        SS_DISABLE = 0x0004,
    }

    enum MINSIGSTKSZ = 512 * 4;
    enum SIGSTKSZ    = (MINSIGSTKSZ + 32768);

    //ucontext_t (defined in core.sys.posix.ucontext)
    //mcontext_t (defined in core.sys.posix.ucontext)

    struct stack_t
    {
        void*   ss_sp;
        size_t  ss_size;
        int     ss_flags;
    }

    struct sigstack
    {
        void*   ss_sp;
        int     ss_onstack;
    }

    //sigfn_t bsd_signal(int sig, sigfn_t func);
    sigfn_t sigset(int sig, sigfn_t func);

  nothrow:
  @nogc:
    //sigfn_t2 bsd_signal(int sig, sigfn_t2 func);
    sigfn_t2 sigset(int sig, sigfn_t2 func);

    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int sighold(int);
    int sigignore(int);
    int siginterrupt(int, int);
    int sigpause(int);
    int sigrelse(int);
}
else version (NetBSD)
{
    enum
    {
        SS_ONSTACK = 0x0001,
        SS_DISABLE = 0x0004,
    }

    enum MINSIGSTKSZ = 8192;
    enum SIGSTKSZ    = (MINSIGSTKSZ + 32768);

    //ucontext_t (defined in core.sys.posix.ucontext)
    //mcontext_t (defined in core.sys.posix.ucontext)

    struct stack_t
    {
        void*   ss_sp;
        size_t  ss_size;
        int     ss_flags;
    }

    struct sigstack
    {
        void*   ss_sp;
        int     ss_onstack;
    }

    //sigfn_t bsd_signal(int sig, sigfn_t func);
    sigfn_t sigset(int sig, sigfn_t func);

  nothrow:
  @nogc:
    //sigfn_t2 bsd_signal(int sig, sigfn_t2 func);
    sigfn_t2 sigset(int sig, sigfn_t2 func);

    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int sighold(int);
    int sigignore(int);
    int siginterrupt(int, int);
    int sigpause(int);
    int sigrelse(int);
}
else version (OpenBSD)
{
    enum
    {
        SS_ONSTACK = 0x0001,
        SS_DISABLE = 0x0004,
    }

    enum MINSIGSTKSZ = 8192;
    enum SIGSTKSZ    = (MINSIGSTKSZ + 32768);

    //ucontext_t (defined in core.sys.posix.ucontext)
    //mcontext_t (defined in core.sys.posix.ucontext)

    struct stack_t
    {
        void*   ss_sp;
        size_t  ss_size;
        int     ss_flags;
    }

  nothrow:
  @nogc:
    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int siginterrupt(int, int);
    int sigpause(int);
}
else version (DragonFlyBSD)
{
    enum
    {
        SS_ONSTACK = 0x0001,
        SS_DISABLE = 0x0004,
    }

    enum MINSIGSTKSZ = 8192;
    enum SIGSTKSZ    = (MINSIGSTKSZ + 32768);

    //ucontext_t (defined in core.sys.posix.ucontext)
    //mcontext_t (defined in core.sys.posix.ucontext)

    struct stack_t
    {
        void*   ss_sp;
        size_t  ss_size;
        int     ss_flags;
    }

    struct sigstack
    {
        void*   ss_sp;
        int     ss_onstack;
    }

    //sigfn_t bsd_signal(int sig, sigfn_t func);
    sigfn_t sigset(int sig, sigfn_t func);

  nothrow:
  @nogc:
    //sigfn_t2 bsd_signal(int sig, sigfn_t2 func);
    sigfn_t2 sigset(int sig, sigfn_t2 func);

    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int sighold(int);
    int sigignore(int);
    int siginterrupt(int, int);
    int sigpause(int);
    int sigrelse(int);
}
else version (Solaris)
{
    enum
    {
        SS_ONSTACK = 0x0001,
        SS_DISABLE = 0x0002,
    }

    enum MINSIGSTKSZ = 2048;
    enum SIGSTKSZ = 8192;

    struct stack_t
    {
        void* ss_sp;
        size_t ss_size;
        int ss_flags;
    }

    struct sigstack
    {
        void* ss_sp;
        int ss_onstack;
    }

    sigfn_t sigset(int sig, sigfn_t func);

  nothrow:
  @nogc:
    sigfn_t2 sigset(int sig, sigfn_t2 func);

    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int sighold(int);
    int sigignore(int);
    int siginterrupt(int, int);
    int sigpause(int);
    int sigrelse(int);
}
else version (CRuntime_Bionic)
{
    enum SS_ONSTACK     = 1;
    enum SS_DISABLE     = 2;
    enum MINSIGSTKSZ    = 2048;
    enum SIGSTKSZ       = 8192;

    struct stack_t
    {
        void*   ss_sp;
        int     ss_flags;
        size_t  ss_size;
    }

    sigfn_t bsd_signal(int, sigfn_t);

  nothrow:
  @nogc:
    sigfn_t2 bsd_signal(int, sigfn_t2);

    int killpg(int, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int siginterrupt(int, int);
}
else version (CRuntime_Musl)
{
    enum SS_ONSTACK = 1;
    enum SS_DISABLE = 2;

    version (ARM)
    {
        enum MINSIGSTKSZ = 2048;
        enum SIGSTKSZ    = 8192;
    }
    else version (AArch64)
    {
        enum MINSIGSTKSZ = 6144;
        enum SIGSTKSZ    = 12288;
    }
    else version (IBMZ_Any)
    {
        enum MINSIGSTKSZ = 4096;
        enum SIGSTKSZ    = 10240;
    }
    else version (MIPS_Any)
    {
        enum MINSIGSTKSZ = 2048;
        enum SIGSTKSZ    = 8192;
    }
    else version (PPC_Any)
    {
        enum MINSIGSTKSZ = 4096;
        enum SIGSTKSZ    = 10240;
    }
    else version (X86_Any)
    {
        enum MINSIGSTKSZ = 2048;
        enum SIGSTKSZ    = 8192;
    }
    else
        static assert(0, "unimplemented");

    //ucontext_t (defined in core.sys.posix.ucontext)
    //mcontext_t (defined in core.sys.posix.ucontext)

    version (MIPS_Any)
    {
        struct stack_t
        {
            void*  ss_sp;
            size_t ss_size;
            int    ss_flags;
        }
    }
    else
    {
        struct stack_t
        {
            void*  ss_sp;
            int    ss_flags;
            size_t ss_size;
        }
    }

    sigfn_t bsd_signal(int sig, sigfn_t func);
    sigfn_t sigset(int sig, sigfn_t func);

  nothrow:
  @nogc:
    sigfn_t2 bsd_signal(int sig, sigfn_t2 func);
    sigfn_t2 sigset(int sig, sigfn_t2 func);

    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int sighold(int);
    int sigignore(int);
    int siginterrupt(int, int);
    int sigpause(int);
    int sigrelse(int);
}
else version (CRuntime_UClibc)
{
    enum SS_ONSTACK     = 1;
    enum SS_DISABLE     = 2;
    enum MINSIGSTKSZ    = 2048;
    enum SIGSTKSZ       = 8192;

    version (MIPS32)
    {
        struct stack_t
        {
            void *ss_sp;
            size_t ss_size;
            int ss_flags;
        }
    }
    else
    {
        struct stack_t
        {
            void*   ss_sp;
            int     ss_flags;
            size_t  ss_size;
        }
     }

    struct sigstack
    {
        void*   ss_sp;
        int     ss_onstack;
    }

    sigfn_t sigset(int sig, sigfn_t func);

  nothrow:
  @nogc:
    sigfn_t2 sigset(int sig, sigfn_t2 func);

    int killpg(pid_t, int);
    int sigaltstack(const scope stack_t*, stack_t*);
    int sighold(int);
    int sigignore(int);
    int siginterrupt(int, int);
    int sigpause(int);
    int sigrelse(int);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Realtime Signals (RTS)
//
/*
struct sigevent
{
    int             sigev_notify;
    int             sigev_signo;
    sigval          sigev_value;
    void(*)(sigval) sigev_notify_function;
    pthread_attr_t* sigev_notify_attributes;
}
*/

nothrow:
@nogc:

version (linux)
{
    private enum __SIGEV_MAX_SIZE = 64;

    static if ( __WORDSIZE == 64 )
    {
        private enum __SIGEV_PAD_SIZE = ((__SIGEV_MAX_SIZE / int.sizeof) - 4);
    }
    else
    {
        private enum __SIGEV_PAD_SIZE = ((__SIGEV_MAX_SIZE / int.sizeof) - 3);
    }

    struct sigevent
    {
        sigval      sigev_value;
        int         sigev_signo;
        int         sigev_notify;

        union
        {
            int[__SIGEV_PAD_SIZE] _pad;
            pid_t                 _tid;

            struct
            {
                void function(sigval) sigev_notify_function;
                void*                 sigev_notify_attributes;
            }
        }
    }
}
else version (FreeBSD)
{
    struct sigevent
    {
        int             sigev_notify;
        int             sigev_signo;
        sigval          sigev_value;
        union
        {
            lwpid_t _threadid;
            struct
            {
                void function(sigval) sigev_notify_function;
                void* sigev_notify_attributes;
            }
            c_long[8] __spare__;
        }
    }
}
else version (NetBSD)
{
    struct sigevent
    {
        int             sigev_notify;
        int             sigev_signo;
        sigval          sigev_value;
        void function(sigval) sigev_notify_function;
        void /* pthread_attr_t */*sigev_notify_attributes;
    }
}
else version (OpenBSD)
{
    // OpenBSD does not implement sigevent.
    alias sigevent = void;
}
else version (DragonFlyBSD)
{
    union  _sigev_un_t
    {
        int                       sigev_signo;
        int                       sigev_notify_kqueue;
        void /*pthread_attr_t*/ * sigev_notify_attributes;
    }
    union _sigval_t
    {
        int                       sival_int;
        void                    * sival_ptr;
        int                       sigval_int;
        void                    * sigval_ptr;
    }
    struct sigevent
    {
        int                       sigev_notify;
        _sigev_un_t               sigev_un;
        _sigval_t                 sigev_value;
        void function(_sigval_t)  sigev_notify_function;
    }
}
else version (Darwin)
{
    struct sigevent
    {
        int sigev_notify;
        int sigev_signo;
        sigval sigev_value;
        void function(sigval) sigev_notify_function;
        pthread_attr_t* sigev_notify_attributes;
    }
}
else version (Solaris)
{
    struct sigevent
    {
        int sigev_notify;
        int sigev_signo;
        sigval sigev_value;
        void function(sigval) sigev_notify_function;
        pthread_attr_t* sigev_notify_attributes;
        int __sigev_pad2;
    }
}
else
{
    static assert(false, "Unsupported platform");
}

/*
int sigqueue(pid_t, int, const sigval);
int sigtimedwait(const scope sigset_t*, siginfo_t*, const scope timespec*);
int sigwaitinfo(const scope sigset_t*, siginfo_t*);
*/

nothrow:
@nogc:

version (CRuntime_Glibc)
{
    int sigqueue(pid_t, int, const sigval);
    int sigtimedwait(const scope sigset_t*, siginfo_t*, const scope timespec*);
    int sigwaitinfo(const scope sigset_t*, siginfo_t*);
}
else version (FreeBSD)
{
    int sigqueue(pid_t, int, const sigval);
    int sigtimedwait(const scope sigset_t*, siginfo_t*, const scope timespec*);
    int sigwaitinfo(const scope sigset_t*, siginfo_t*);
}
else version (NetBSD)
{
    int sigqueue(pid_t, int, const sigval);
    int sigtimedwait(const scope sigset_t*, siginfo_t*, const scope timespec*);
    int sigwaitinfo(const scope sigset_t*, siginfo_t*);
}
else version (OpenBSD)
{
}
else version (DragonFlyBSD)
{
    int sigqueue(pid_t, int, const sigval);
    int sigtimedwait(const scope sigset_t*, siginfo_t*, const scope timespec*);
    int sigwaitinfo(const scope sigset_t*, siginfo_t*);
}
else version (Darwin)
{
}
else version (Solaris)
{
    int sigqueue(pid_t, int, const sigval);
    int sigtimedwait(const scope sigset_t*, siginfo_t*, const scope timespec*);
    int sigwaitinfo(const scope sigset_t*, siginfo_t*);
}
else version (CRuntime_Bionic)
{
}
else version (CRuntime_Musl)
{
    int sigqueue(pid_t, int, const sigval);
    pragma(mangle, muslRedirTime64Mangle!("sigtimedwait", "__sigtimedwait_time64"))
    int sigtimedwait(const scope sigset_t*, siginfo_t*, const scope timespec*);
    int sigwaitinfo(const scope sigset_t*, siginfo_t*);
}
else version (CRuntime_UClibc)
{
    int sigqueue(pid_t, int, const sigval);
    int sigtimedwait(const scope sigset_t*, siginfo_t*, const scope timespec*);
    int sigwaitinfo(const scope sigset_t*, siginfo_t*);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Threads (THR)
//
/*
int pthread_kill(pthread_t, int);
int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
*/

version (CRuntime_Glibc)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (Darwin)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (FreeBSD)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (NetBSD)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (OpenBSD)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (DragonFlyBSD)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (Solaris)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (CRuntime_Bionic)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (CRuntime_Musl)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
}
else version (CRuntime_UClibc)
{
    int pthread_kill(pthread_t, int);
    int pthread_sigmask(int, const scope sigset_t*, sigset_t*);
    int pthread_sigqueue(pthread_t, int, sigval);
}
else
{
    static assert(false, "Unsupported platform");
}
