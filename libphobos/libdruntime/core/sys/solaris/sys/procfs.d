/**
  * D header file for Solaris sys/procfs.h.
  *
  * Copyright: Copyright © 2025, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.solaris.sys.procfs;

version (Solaris):
extern (C):
nothrow:
@nogc:

import core.stdc.config : c_long, c_ulong;
import core.stdc.stdint : uintptr_t;
import core.sys.posix.signal : sigaction_t, siginfo_t, sigset_t, stack_t;
import core.sys.posix.time : timestruc_t;

version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;

version (X86_Any)
{
    /*
     * Holds one i386 or and64 instruction
     */
    alias instr_t = ubyte;

    public import core.sys.solaris.sys.regset :
        NPRGREG = _NGREG,
        prgreg_t = greg_t,
        prgregset_t = gregset_t,
        prfpregset_t = fpregset_t;

    /*
     * The following defines are for portability
     */
    version (X86_64)
    {
        public import core.sys.solaris.sys.regset :
            R_PC = REG_RIP,
            R_PS = REG_RFL,
            R_SP = REG_RSP,
            R_FP = REG_RBP,
            R_R0 = REG_RAX,
            R_R1 = REG_RDX;
    }
    else
    {
        public import core.sys.solaris.sys.regset :
            R_PC = REG_EIP,
            R_PS = REG_EFL,
            R_SP = REG_UESP,
            R_FP = REG_EBP,
            R_R0 = REG_EAX,
            R_R1 = REG_EDX;
    }
}
else version (SPARC_Any)
{
    import core.sys.solaris.sys.regset : fq;

    /*
     * Holds one sparc instruction, both ILP32 and LP64
     */
    alias instr_t = uint;

    /*
     * General register access (sparc).
     * Registers are 32 bits for ILP32, 64 bits for LP64.
     */
    enum NPRGREG = 38;

    version (D_LP64)
        alias prgreg_t = c_long;
    else
        alias prgreg_t = int;

    alias prgregset_t = prgreg_t[NPRGREG];

    /*
     * Floating-point register access (sparc FPU).
     */
    version (SPARC64)
    {
        struct prfpregset_t
        {
            union
            {
                uint[32]   pr_regs;
                double[32] pr_dregs;
                real[16]   pr_qregs;
            }
            ulong pr_filler;
            ulong pr_fsr;
            ubyte pr_qcnt;
            ubyte pr_q_entrysize;
            ubyte pr_en;
            byte[13] pr_pad;
            fq[16] pr_q;
        }
    }
    else
    {
        struct prfpregset_t
        {
            union
            {
                uint[32]   pr_regs;
                double[16] pr_dregs;
            }
            uint  pr_filler;
            uint  pr_fsr;
            ubyte pr_qcnt;
            ubyte pr_q_entrysize;
            ubyte pr_en;
            fq[32] pr_q;
        }
    }

    enum R_G0 = 0;
    enum R_G1 = 1;
    enum R_G2 = 2;
    enum R_G3 = 3;
    enum R_G4 = 4;
    enum R_G5 = 5;
    enum R_G6 = 6;
    enum R_G7 = 7;
    enum R_O0 = 8;
    enum R_O1 = 9;
    enum R_O2 = 10;
    enum R_O3 = 11;
    enum R_O4 = 12;
    enum R_O5 = 13;
    enum R_O6 = 14;
    enum R_O7 = 15;
    enum R_L0 = 16;
    enum R_L1 = 17;
    enum R_L2 = 18;
    enum R_L3 = 19;
    enum R_L4 = 20;
    enum R_L5 = 21;
    enum R_L6 = 22;
    enum R_L7 = 23;
    enum R_I0 = 24;
    enum R_I1 = 25;
    enum R_I2 = 26;
    enum R_I3 = 27;
    enum R_I4 = 28;
    enum R_I5 = 29;
    enum R_I6 = 30;
    enum R_I7 = 31;
    enum R_PC = 33;
    enum R_nPC = 34;
    enum R_Y  = 35;

    version (SPARC64)
    {
        enum R_CCR = 32;
        enum R_ASI = 36;
        enum R_FPRS = 37;
        enum R_PS = R_CCR;
    }
    else
    {
        enum R_PSR = 32;
        enum R_WIM = 36;
        enum R_TBR = 37;
        enum R_PS = R_PSR;
    }

    enum R_SP = R_O6;
    enum R_FP = R_I6;
    enum R_R0 = R_O0;
    enum R_R1 = R_O1;
}

/*
 * lwp status file.  /proc/<pid>/lwp/<lwpid>/lwpstatus
 */
private enum PRCLSZ    = 8;     // maximum size of scheduling class name
private enum PRSYSARGS = 8;     // maximum number of syscall arguments

struct lwpstatus_t
{
    int     pr_flags;               // flags (see below)
    int     pr_lwpid;               // specific lwp identifier
    short   pr_why;                 // reason for lwp stop, if stopped
    short   pr_what;                // more detailed reason
    short   pr_cursig;              // current signal, if any
    ubyte   pr_adi;
    byte    pr_pad1;
    siginfo_t pr_info;              // info associated with signal or fault
    sigset_t pr_lwppend;            // set of signals pending to the lwp
    sigset_t pr_lwphold;            // set of signals blocked by the lwp
    sigaction_t pr_action;          // signal action for current signal
    stack_t pr_altstack;            // alternate signal stack info
    uintptr_t pr_oldcontext;        // address of previous ucontext
    short   pr_syscall;             // system call number (if in syscall)
    short   pr_nsysarg;             // number of arguments to this syscall
    int     pr_errno;               // errno for failed syscall, 0 if successful
    c_long[PRSYSARGS] pr_sysarg;    // arguments to this syscall
    c_long    pr_rval1;             // primary syscall return value
    c_long    pr_rval2;             // second syscall return value, if any
    char[PRCLSZ] pr_clname = void;  // scheduling class name
    timestruc_t pr_tstamp;          // real-time time stamp of stop
    timestruc_t pr_utime;           // lwp user cpu time
    timestruc_t pr_stime;           // lwp system cpu time
    int[11 - 2 * timestruc_t.sizeof / int.sizeof] pr_filler;
    int     pr_errpriv;             // missing privilege
    uintptr_t pr_ustack;            // address of stack boundary data (stack_t)
    c_ulong pr_instr;               // current instruction
    prgregset_t pr_reg;             // general registers
    prfpregset_t pr_fpreg;          // floating-point registers
}

/*
 * pr_flags (same values appear in both pstatus_t and lwpstatus_t pr_flags).
 *
 * These flags do *not* apply to psinfo_t.pr_flag or lwpsinfo_t.pr_flag
 * (which are both deprecated).
 */
// The following flags apply to the specific or representative lwp
enum PR_STOPPED = 0x00000001;   // lwp is stopped
enum PR_ISTOP   = 0x00000002;   // lwp is stopped on an event of interest
enum PR_DSTOP   = 0x00000004;   // lwp has a stop directive in effect
enum PR_STEP    = 0x00000008;   // lwp has a single-step directive in effect
enum PR_ASLEEP  = 0x00000010;   // lwp is sleeping in a system call
enum PR_PCINVAL = 0x00000020;   // contents of pr_instr undefined
enum PR_ASLWP   = 0x00000040;   // obsolete flag; never set
enum PR_AGENT   = 0x00000080;   // this lwp is the /proc agent lwp
enum PR_DETACH  = 0x00000100;   // this is a detached lwp
enum PR_DAEMON  = 0x00000200;   // this is a daemon lwp
enum PR_IDLE    = 0x00000400;   // lwp is a cpu's idle thread
// The following flags apply to the process, not to an individual lwp
enum PR_ISSYS   = 0x00001000;   // this is a system process
enum PR_VFORKP  = 0x00002000;   // process is the parent of a vfork()d child
enum PR_ORPHAN  = 0x00004000;   // process's process group is orphaned
enum PR_NOSIGCHLD = 0x00008000; // process will not generate SIGCHLD on exit
enum PR_WAITPID = 0x00010000;   // only waitid(P_PID, pid) can reap the child
// The following process flags are modes settable by PCSET/PCUNSET
enum PR_FORK    = 0x00100000;   // inherit-on-fork is in effect
enum PR_RLC     = 0x00200000;   // run-on-last-close is in effect
enum PR_KLC     = 0x00400000;   // kill-on-last-close is in effect
enum PR_ASYNC   = 0x00800000;   // asynchronous-stop is in effect
enum PR_MSACCT  = 0x01000000;   // micro-state usage accounting is in effect
enum PR_BPTADJ  = 0x02000000;   // breakpoint trap pc adjustment is in effect
enum PR_PTRACE  = 0x04000000;   // ptrace-compatibility mode is in effect
enum PR_MSFORK  = 0x08000000;   // micro-state accounting inherited on fork
