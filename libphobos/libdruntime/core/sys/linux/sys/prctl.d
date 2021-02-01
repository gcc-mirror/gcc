/**
 * D header file for GNU/Linux.
 *
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Arun Chandrasekaran <aruncxy@gmail.com>
 */
module core.sys.linux.sys.prctl;

version (linux):
extern (C):
@system:
@nogc:
nothrow:

enum: uint
{
    PR_SET_PDEATHSIG                    = 1,
    PR_GET_PDEATHSIG                    = 2,

    PR_GET_DUMPABLE                     = 3,
    PR_SET_DUMPABLE                     = 4,

    PR_GET_UNALIGN                      = 5,
    PR_SET_UNALIGN                      = 6,

    PR_UNALIGN_NOPRINT                  = 1,
    PR_UNALIGN_SIGBUS                   = 2,

    PR_GET_KEEPCAPS                     = 7,
    PR_SET_KEEPCAPS                     = 8,

    PR_GET_FPEMU                        = 9,
    PR_SET_FPEMU                        = 10,
    PR_FPEMU_NOPRINT                    = 1,
    PR_FPEMU_SIGFPE                     = 2,

    PR_GET_FPEXC                        = 11,
    PR_SET_FPEXC                        = 12,
    PR_FP_EXC_SW_ENABLE                 = 0x80,
    PR_FP_EXC_DIV                       = 0x010000,
    PR_FP_EXC_OVF                       = 0x020000,
    PR_FP_EXC_UND                       = 0x040000,
    PR_FP_EXC_RES                       = 0x080000,
    PR_FP_EXC_INV                       = 0x100000,
    PR_FP_EXC_DISABLED                  = 0,
    PR_FP_EXC_NONRECOV                  = 1,
    PR_FP_EXC_ASYNC                     = 2,
    PR_FP_EXC_PRECISE                   = 3,

    PR_GET_TIMING                       = 13,
    PR_SET_TIMING                       = 14,
    PR_TIMING_STATISTICAL               = 0,
    PR_TIMING_TIMESTAMP                 = 1,

    PR_SET_NAME                         = 15,
    PR_GET_NAME                         = 16,

    PR_GET_ENDIAN                       = 19,
    PR_SET_ENDIAN                       = 20,
    PR_ENDIAN_BIG                       = 0,
    PR_ENDIAN_LITTLE                    = 1,
    PR_ENDIAN_PPC_LITTLE                = 2,

    PR_GET_SECCOMP                      = 21,
    PR_SET_SECCOMP                      = 22,

    PR_CAPBSET_READ                     = 23,
    PR_CAPBSET_DROP                     = 24,

    PR_GET_TSC                          = 25,
    PR_SET_TSC                          = 26,
    PR_TSC_ENABLE                       = 1,
    PR_TSC_SIGSEGV                      = 2,

    PR_GET_SECUREBITS                   = 27,
    PR_SET_SECUREBITS                   = 28,


    PR_SET_TIMERSLACK                   = 29,
    PR_GET_TIMERSLACK                   = 30,

    PR_TASK_PERF_EVENTS_DISABLE         = 31,
    PR_TASK_PERF_EVENTS_ENABLE          = 32,


    PR_MCE_KILL                         = 33,
    PR_MCE_KILL_CLEAR                   = 0,
    PR_MCE_KILL_SET                     = 1,

    PR_MCE_KILL_LATE                    = 0,
    PR_MCE_KILL_EARLY                   = 1,
    PR_MCE_KILL_DEFAULT                 = 2,

    PR_MCE_KILL_GET                     = 34,

    PR_SET_MM                           = 35,
    PR_SET_MM_START_CODE                = 1,
    PR_SET_MM_END_CODE                  = 2,
    PR_SET_MM_START_DATA                = 3,
    PR_SET_MM_END_DATA                  = 4,
    PR_SET_MM_START_STACK               = 5,
    PR_SET_MM_START_BRK                 = 6,
    PR_SET_MM_BRK                       = 7,
    PR_SET_MM_ARG_START                 = 8,
    PR_SET_MM_ARG_END                   = 9,
    PR_SET_MM_ENV_START                 = 10,
    PR_SET_MM_ENV_END                   = 11,
    PR_SET_MM_AUXV                      = 12,
    PR_SET_MM_EXE_FILE                  = 13,
    PR_SET_MM_MAP                       = 14,
    PR_SET_MM_MAP_SIZE                  = 15,

    PR_SET_PTRACER                      = 0x59616d61,
    PR_SET_PTRACER_ANY                  = (cast (uint)-1),

    PR_SET_CHILD_SUBREAPER              = 36,
    PR_GET_CHILD_SUBREAPER              = 37,

    PR_SET_NO_NEW_PRIVS                 = 38,
    PR_GET_NO_NEW_PRIVS                 = 39,

    PR_GET_TID_ADDRESS                  = 40,

    PR_SET_THP_DISABLE                  = 41,
    PR_GET_THP_DISABLE                  = 42,
}

struct prctl_mm_map
{
    ulong    start_code;
    ulong    end_code;
    ulong    start_data;
    ulong    end_data;
    ulong    start_brk;
    ulong    brk;
    ulong    start_stack;
    ulong    arg_start;
    ulong    arg_end;
    ulong    env_start;
    ulong    env_end;
    ulong*   auxv;
    uint     auxv_size;
    uint     exe_fd;
}

int prctl(int option, size_t arg2, size_t arg3, size_t arg4, size_t arg5);

//
// Example usage to set and get the task name.
//
// byte[16] oldname = cast(byte[]) "1234567890123456";
// oldname[oldname.length-1] = 0;
// prctl(PR_SET_NAME, cast(size_t) oldname.ptr, cast(size_t) null, cast(size_t) null, cast(size_t) null);
// byte[16] newname;
// prctl(PR_GET_NAME, cast(size_t) newname.ptr, cast(size_t) null, cast(size_t) null, cast(size_t) null);
// int i;
// foreach (b; newname)
// {
//         assert(b == oldname[i]);
//             i++;
// }
// writeln(cast(string) newname);
