/**
  * D header file for perf_event_open system call.
  *
  * Converted from linux userspace header, comments included.
  *
  * Authors: Max Haughton
  */
module core.sys.linux.perf_event;
version (linux)  : extern (C):
@nogc:
nothrow:

import core.sys.posix.sys.ioctl;
import core.sys.posix.unistd;

version (HPPA)    version = HPPA_Any;
version (HPPA64)  version = HPPA_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;

version (X86_64)
{
    version (D_X32)
        enum __NR_perf_event_open = 0x40000000 + 298;
    else
        enum __NR_perf_event_open = 298;
}
else version (X86)
{
        enum __NR_perf_event_open = 336;
}
else version (ARM)
{
        enum __NR_perf_event_open = 364;
}
else version (AArch64)
{
        enum __NR_perf_event_open = 241;
}
else version (HPPA_Any)
{
        enum __NR_perf_event_open = 318;
}
else version (IBMZ_Any)
{
        enum __NR_perf_event_open = 331;
}
else version (MIPS32)
{
        enum __NR_perf_event_open = 4333;
}
else version (MIPS64)
{
    version (MIPS_N32)
        enum __NR_perf_event_open = 6296;
    else version (MIPS_N64)
        enum __NR_perf_event_open = 5292;
    else
        static assert(0, "Architecture not supported");
}
else version (PPC_Any)
{
        enum __NR_perf_event_open = 319;
}
else version (RISCV_Any)
{
        enum __NR_perf_event_open = 241;
}
else version (SPARC_Any)
{
        enum __NR_perf_event_open = 327;
}
else version (LoongArch64)
{
        enum __NR_perf_event_open = 241;
}
else
{
        static assert(0, "Architecture not supported");
}
extern (C) extern long syscall(long __sysno, ...);
static long perf_event_open(perf_event_attr* hw_event, pid_t pid, int cpu, int group_fd, ulong flags)
{
        return syscall(__NR_perf_event_open, hw_event, pid, cpu, group_fd, flags);
}
/*
 * User-space ABI bits:
 */

/**
 * attr.type
 */
enum perf_type_id
{
        PERF_TYPE_HARDWARE = 0,
        PERF_TYPE_SOFTWARE = 1,
        PERF_TYPE_TRACEPOINT = 2,
        PERF_TYPE_HW_CACHE = 3,
        PERF_TYPE_RAW = 4,
        PERF_TYPE_BREAKPOINT = 5,

        PERF_TYPE_MAX = 6 /* non-ABI */
}
/**
 * Generalized performance event event_id types, used by the
 * attr.event_id parameter of the sys_perf_event_open()
 * syscall:
 */
enum perf_hw_id
{
        ///
        PERF_COUNT_HW_CPU_CYCLES = 0,
        ///
        PERF_COUNT_HW_INSTRUCTIONS = 1,
        ///
        PERF_COUNT_HW_CACHE_REFERENCES = 2,
        ///
        PERF_COUNT_HW_CACHE_MISSES = 3,
        ///
        PERF_COUNT_HW_BRANCH_INSTRUCTIONS = 4,
        ///
        PERF_COUNT_HW_BRANCH_MISSES = 5,
        ///
        PERF_COUNT_HW_BUS_CYCLES = 6,
        ///
        PERF_COUNT_HW_STALLED_CYCLES_FRONTEND = 7,
        ///
        PERF_COUNT_HW_STALLED_CYCLES_BACKEND = 8,
        ///
        PERF_COUNT_HW_REF_CPU_CYCLES = 9,
        ///
        PERF_COUNT_HW_MAX = 10 /* non-ABI */
}

/**
 * Generalized hardware cache events:
 *
 *       { L1-D, L1-I, LLC, ITLB, DTLB, BPU, NODE } x
 *       { read, write, prefetch } x
 *       { accesses, misses }
 */
enum perf_hw_cache_id
{
        ///
        PERF_COUNT_HW_CACHE_L1D = 0,
        ///
        PERF_COUNT_HW_CACHE_L1I = 1,
        ///
        PERF_COUNT_HW_CACHE_LL = 2,
        ///
        PERF_COUNT_HW_CACHE_DTLB = 3,
        ///
        PERF_COUNT_HW_CACHE_ITLB = 4,
        ///
        PERF_COUNT_HW_CACHE_BPU = 5,
        ///
        PERF_COUNT_HW_CACHE_NODE = 6,
        ///
        PERF_COUNT_HW_CACHE_MAX = 7 /* non-ABI */
}
///
enum perf_hw_cache_op_id
{
        ///
        PERF_COUNT_HW_CACHE_OP_READ = 0,
        ///
        PERF_COUNT_HW_CACHE_OP_WRITE = 1,
        ///
        PERF_COUNT_HW_CACHE_OP_PREFETCH = 2,
        ///
        PERF_COUNT_HW_CACHE_OP_MAX = 3 /* non-ABI */
}
///
enum perf_hw_cache_op_result_id
{
        ///
        PERF_COUNT_HW_CACHE_RESULT_ACCESS = 0,
        ///
        PERF_COUNT_HW_CACHE_RESULT_MISS = 1,
        ///
        PERF_COUNT_HW_CACHE_RESULT_MAX = 2 /* non-ABI */
}

/**
 * Special "software" events provided by the kernel, even if the hardware
 * does not support performance events. These events measure various
 * physical and sw events of the kernel (and allow the profiling of them as
 * well):
 */
enum perf_sw_ids
{
        ///
        PERF_COUNT_SW_CPU_CLOCK = 0,
        ///
        PERF_COUNT_SW_TASK_CLOCK = 1,
        ///
        PERF_COUNT_SW_PAGE_FAULTS = 2,
        ///
        PERF_COUNT_SW_CONTEXT_SWITCHES = 3,
        ///
        PERF_COUNT_SW_CPU_MIGRATIONS = 4,
        ///
        PERF_COUNT_SW_PAGE_FAULTS_MIN = 5,
        ///
        PERF_COUNT_SW_PAGE_FAULTS_MAJ = 6,
        ///
        PERF_COUNT_SW_ALIGNMENT_FAULTS = 7,
        ///
        PERF_COUNT_SW_EMULATION_FAULTS = 8,
        ///
        PERF_COUNT_SW_DUMMY = 9,
        ///
        PERF_COUNT_SW_BPF_OUTPUT = 10,
        ///
        PERF_COUNT_SW_MAX = 11 /* non-ABI */
}

/**
 * Bits that can be set in attr.sample_type to request information
 * in the overflow packets.
 */
enum perf_event_sample_format
{
        ///
        PERF_SAMPLE_IP = 1U << 0,
        ///
        PERF_SAMPLE_TID = 1U << 1,
        ///
        PERF_SAMPLE_TIME = 1U << 2,
        ///
        PERF_SAMPLE_ADDR = 1U << 3,
        ///
        PERF_SAMPLE_READ = 1U << 4,
        ///
        PERF_SAMPLE_CALLCHAIN = 1U << 5,
        ///
        PERF_SAMPLE_ID = 1U << 6,
        ///
        PERF_SAMPLE_CPU = 1U << 7,
        ///
        PERF_SAMPLE_PERIOD = 1U << 8,
        ///
        PERF_SAMPLE_STREAM_ID = 1U << 9,
        ///
        PERF_SAMPLE_RAW = 1U << 10,
        ///
        PERF_SAMPLE_BRANCH_STACK = 1U << 11,
        ///
        PERF_SAMPLE_REGS_USER = 1U << 12,
        ///
        PERF_SAMPLE_STACK_USER = 1U << 13,
        ///
        PERF_SAMPLE_WEIGHT = 1U << 14,
        ///
        PERF_SAMPLE_DATA_SRC = 1U << 15,
        ///
        PERF_SAMPLE_IDENTIFIER = 1U << 16,
        ///
        PERF_SAMPLE_TRANSACTION = 1U << 17,
        ///
        PERF_SAMPLE_REGS_INTR = 1U << 18,
        ///
        PERF_SAMPLE_PHYS_ADDR = 1U << 19,
        ///
        PERF_SAMPLE_MAX = 1U << 20 /* non-ABI */
}

/**
 * values to program into branch_sample_type when PERF_SAMPLE_BRANCH is set
 *
 * If the user does not pass priv level information via branch_sample_type,
 * the kernel uses the event's priv level. Branch and event priv levels do
 * not have to match. Branch priv level is checked for permissions.
 *
 * The branch types can be combined, however BRANCH_ANY covers all types
 * of branches and therefore it supersedes all the other types.
 */
enum perf_branch_sample_type_shift
{
        PERF_SAMPLE_BRANCH_USER_SHIFT = 0, /** user branches */
        PERF_SAMPLE_BRANCH_KERNEL_SHIFT = 1, /** kernel branches */
        PERF_SAMPLE_BRANCH_HV_SHIFT = 2, /** hypervisor branches */

        PERF_SAMPLE_BRANCH_ANY_SHIFT = 3, /** any branch types */
        PERF_SAMPLE_BRANCH_ANY_CALL_SHIFT = 4, /** any call branch */
        PERF_SAMPLE_BRANCH_ANY_RETURN_SHIFT = 5, /** any return branch */
        PERF_SAMPLE_BRANCH_IND_CALL_SHIFT = 6, /** indirect calls */
        PERF_SAMPLE_BRANCH_ABORT_TX_SHIFT = 7, /** transaction aborts */
        PERF_SAMPLE_BRANCH_IN_TX_SHIFT = 8, /** in transaction */
        PERF_SAMPLE_BRANCH_NO_TX_SHIFT = 9, /** not in transaction */
        PERF_SAMPLE_BRANCH_COND_SHIFT = 10, /** conditional branches */

        PERF_SAMPLE_BRANCH_CALL_STACK_SHIFT = 11, /** call/ret stack */
        PERF_SAMPLE_BRANCH_IND_JUMP_SHIFT = 12, /** indirect jumps */
        PERF_SAMPLE_BRANCH_CALL_SHIFT = 13, /** direct call */

        PERF_SAMPLE_BRANCH_NO_FLAGS_SHIFT = 14, /** no flags */
        PERF_SAMPLE_BRANCH_NO_CYCLES_SHIFT = 15, /** no cycles */

        PERF_SAMPLE_BRANCH_TYPE_SAVE_SHIFT = 16, /** save branch type */

        PERF_SAMPLE_BRANCH_MAX_SHIFT = 17 /** non-ABI */
}
///
enum perf_branch_sample_type
{
        PERF_SAMPLE_BRANCH_USER = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_USER_SHIFT,
        PERF_SAMPLE_BRANCH_KERNEL = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_KERNEL_SHIFT,
        PERF_SAMPLE_BRANCH_HV = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_HV_SHIFT,
        PERF_SAMPLE_BRANCH_ANY = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_ANY_SHIFT,
        PERF_SAMPLE_BRANCH_ANY_CALL = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_ANY_CALL_SHIFT,
        PERF_SAMPLE_BRANCH_ANY_RETURN = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_ANY_RETURN_SHIFT,
        PERF_SAMPLE_BRANCH_IND_CALL = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_IND_CALL_SHIFT,
        PERF_SAMPLE_BRANCH_ABORT_TX = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_ABORT_TX_SHIFT,
        PERF_SAMPLE_BRANCH_IN_TX = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_IN_TX_SHIFT,
        PERF_SAMPLE_BRANCH_NO_TX = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_NO_TX_SHIFT,
        PERF_SAMPLE_BRANCH_COND = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_COND_SHIFT,
        PERF_SAMPLE_BRANCH_CALL_STACK = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_CALL_STACK_SHIFT,
        PERF_SAMPLE_BRANCH_IND_JUMP = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_IND_JUMP_SHIFT,
        PERF_SAMPLE_BRANCH_CALL = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_CALL_SHIFT,
        PERF_SAMPLE_BRANCH_NO_FLAGS = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_NO_FLAGS_SHIFT,
        PERF_SAMPLE_BRANCH_NO_CYCLES = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_NO_CYCLES_SHIFT,
        PERF_SAMPLE_BRANCH_TYPE_SAVE = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_TYPE_SAVE_SHIFT,
        PERF_SAMPLE_BRANCH_MAX = 1U << perf_branch_sample_type_shift.PERF_SAMPLE_BRANCH_MAX_SHIFT
}

/**
 * Common flow change classification
 */
enum
{
        PERF_BR_UNKNOWN = 0, /** unknown */
        PERF_BR_COND = 1, /** conditional */
        PERF_BR_UNCOND = 2, /** unconditional  */
        PERF_BR_IND = 3, /** indirect */
        PERF_BR_CALL = 4, /** function call */
        PERF_BR_IND_CALL = 5, /** indirect function call */
        PERF_BR_RET = 6, /** function return */
        PERF_BR_SYSCALL = 7, /** syscall */
        PERF_BR_SYSRET = 8, /** syscall return */
        PERF_BR_COND_CALL = 9, /** conditional function call */
        PERF_BR_COND_RET = 10, /** conditional function return */
        PERF_BR_MAX = 11
}

///
enum PERF_SAMPLE_BRANCH_PLM_ALL = perf_branch_sample_type.PERF_SAMPLE_BRANCH_USER
        | perf_branch_sample_type.PERF_SAMPLE_BRANCH_KERNEL
        | perf_branch_sample_type.PERF_SAMPLE_BRANCH_HV;

/**
 * Values to determine ABI of the registers dump.
 */
enum perf_sample_regs_abi
{
        ///
        PERF_SAMPLE_REGS_ABI_NONE = 0,
        ///
        PERF_SAMPLE_REGS_ABI_32 = 1,
        ///
        PERF_SAMPLE_REGS_ABI_64 = 2
}

/**
 * Values for the memory transaction event qualifier, mostly for
 * abort events. Multiple bits can be set.
 */
enum
{
        PERF_TXN_ELISION = 1 << 0, /** From elision */
        PERF_TXN_TRANSACTION = 1 << 1, /** From transaction */
        PERF_TXN_SYNC = 1 << 2, /** Instruction is related */
        PERF_TXN_ASYNC = 1 << 3, /** Instruction not related */
        PERF_TXN_RETRY = 1 << 4, /** Retry possible */
        PERF_TXN_CONFLICT = 1 << 5, /** Conflict abort */
        PERF_TXN_CAPACITY_WRITE = 1 << 6, /** Capacity write abort */
        PERF_TXN_CAPACITY_READ = 1 << 7, /** Capacity read abort */

        PERF_TXN_MAX = 1 << 8, /** non-ABI */

        /** bits 32..63 are reserved for the abort code */

        ///PERF_TXN_ABORT_MASK = 0xffffffff << 32,
        PERF_TXN_ABORT_SHIFT = 32
}

/**
 * The format of the data returned by read() on a perf event fd,
 * as specified by attr.read_format:
 * ---
 * struct read_format {
 *    { u64        value;
 *      { u64        time_enabled; } && PERF_FORMAT_TOTAL_TIME_ENABLED
 *      { u64        time_running; } && PERF_FORMAT_TOTAL_TIME_RUNNING
 *      { u64        id;           } && PERF_FORMAT_ID
 *    } && !PERF_FORMAT_GROUP
 *
 *    { u64        nr;
 *      { u64        time_enabled; } && PERF_FORMAT_TOTAL_TIME_ENABLED
 *      { u64        time_running; } && PERF_FORMAT_TOTAL_TIME_RUNNING
 *      { u64        value;
 *        { u64    id;           } && PERF_FORMAT_ID
 *      }        cntr[nr];
 *    } && PERF_FORMAT_GROUP
 * };
 * ---
 */
enum perf_event_read_format
{
        ///
        PERF_FORMAT_TOTAL_TIME_ENABLED = 1U << 0,
        ///
        PERF_FORMAT_TOTAL_TIME_RUNNING = 1U << 1,
        ///
        PERF_FORMAT_ID = 1U << 2,
        ///
        PERF_FORMAT_GROUP = 1U << 3,
        PERF_FORMAT_MAX = 1U << 4 /** non-ABI */
}

enum PERF_ATTR_SIZE_VER0 = 64; /** sizeof first published struct */
enum PERF_ATTR_SIZE_VER1 = 72; /** add: config2 */
enum PERF_ATTR_SIZE_VER2 = 80; /** add: branch_sample_type */
enum PERF_ATTR_SIZE_VER3 = 96; /** add: sample_regs_user */
/* add: sample_stack_user */
enum PERF_ATTR_SIZE_VER4 = 104; /** add: sample_regs_intr */
enum PERF_ATTR_SIZE_VER5 = 112; /** add: aux_watermark */

/**
 * Hardware event_id to monitor via a performance monitoring event:
 *
 * @sample_max_stack: Max number of frame pointers in a callchain,
 *              should be < /proc/sys/kernel/perf_event_max_stack
 */
struct perf_event_attr
{
        /**
        *Major type: hardware/software/tracepoint/etc.
        */
        uint type;

        /**
        * Size of the attr structure, for fwd/bwd compat.
        */
        uint size;

        /**
        * Type specific configuration information.
        */
        ulong config;
        ///
        union
        {
                ///
                ulong sample_period;
                ///
                ulong sample_freq;
        }
        ///
        ulong sample_type;
        ///
        ulong read_format;

        // mixin(bitfields!(
        //     ulong, "disabled", 1,
        //     ulong, "inherit", 1,
        //     ulong, "pinned", 1,
        //     ulong, "exclusive", 1,
        //     ulong, "exclude_user", 1,
        //     ulong, "exclude_kernel", 1,
        //     ulong, "exclude_hv", 1,
        //     ulong, "exclude_idle", 1,
        //     ulong, "mmap", 1,
        //     ulong, "comm", 1,
        //     ulong, "freq", 1,
        //     ulong, "inherit_stat", 1,
        //     ulong, "enable_on_exec", 1,
        //     ulong, "task", 1,
        //     ulong, "watermark", 1,
        //     ulong, "precise_ip", 2,
        //     ulong, "mmap_data", 1,
        //     ulong, "sample_id_all", 1,
        //     ulong, "exclude_host", 1,
        //     ulong, "exclude_guest", 1,
        //     ulong, "exclude_callchain_kernel", 1,
        //     ulong, "exclude_callchain_user", 1,
        //     ulong, "mmap2", 1,
        //     ulong, "comm_exec", 1,
        //     ulong, "use_clockid", 1,
        //     ulong, "context_switch", 1,
        //     ulong, "write_backward", 1,
        //     ulong, "namespaces", 1,
        //     ulong, "__reserved_1", 35));
        private ulong perf_event_attr_bitmanip;
        ///
        @property ulong disabled() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 1U) >> 0U;
                return cast(ulong) result;
        }
        ///
        @property void disabled(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= disabled_min,
                                "Value is smaller than the minimum value of bitfield 'disabled'");
                assert(v <= disabled_max,
                                "Value is greater than the maximum value of bitfield 'disabled'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 1U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 0U) & 1U));
        }

        enum ulong disabled_min = cast(ulong) 0U;
        enum ulong disabled_max = cast(ulong) 1U;
        ///
        @property ulong inherit() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 2U) >> 1U;
                return cast(ulong) result;
        }
        ///
        @property void inherit(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= inherit_min,
                                "Value is smaller than the minimum value of bitfield 'inherit'");
                assert(v <= inherit_max,
                                "Value is greater than the maximum value of bitfield 'inherit'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 2U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 1U) & 2U));
        }

        enum ulong inherit_min = cast(ulong) 0U;
        enum ulong inherit_max = cast(ulong) 1U;
        ///
        @property ulong pinned() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 4U) >> 2U;
                return cast(ulong) result;
        }
        ///
        @property void pinned(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= pinned_min,
                                "Value is smaller than the minimum value of bitfield 'pinned'");
                assert(v <= pinned_max,
                                "Value is greater than the maximum value of bitfield 'pinned'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 4U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 2U) & 4U));
        }

        enum ulong pinned_min = cast(ulong) 0U;
        enum ulong pinned_max = cast(ulong) 1U;
        ///
        @property ulong exclusive() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 8U) >> 3U;
                return cast(ulong) result;
        }
        ///
        @property void exclusive(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclusive_min,
                                "Value is smaller than the minimum value of bitfield 'exclusive'");
                assert(v <= exclusive_max,
                                "Value is greater than the maximum value of bitfield 'exclusive'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 8U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 3U) & 8U));
        }

        enum ulong exclusive_min = cast(ulong) 0U;
        enum ulong exclusive_max = cast(ulong) 1U;
        ///
        @property ulong exclude_user() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 16U) >> 4U;
                return cast(ulong) result;
        }
        ///
        @property void exclude_user(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclude_user_min,
                                "Value is smaller than the minimum value of bitfield 'exclude_user'");
                assert(v <= exclude_user_max,
                                "Value is greater than the maximum value of bitfield 'exclude_user'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 16U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 4U) & 16U));
        }

        enum ulong exclude_user_min = cast(ulong) 0U;
        enum ulong exclude_user_max = cast(ulong) 1U;
        ///
        @property ulong exclude_kernel() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 32U) >> 5U;
                return cast(ulong) result;
        }
        ///
        @property void exclude_kernel(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclude_kernel_min,
                                "Value is smaller than the minimum value of bitfield 'exclude_kernel'");
                assert(v <= exclude_kernel_max,
                                "Value is greater than the maximum value of bitfield 'exclude_kernel'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 32U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 5U) & 32U));
        }

        enum ulong exclude_kernel_min = cast(ulong) 0U;
        enum ulong exclude_kernel_max = cast(ulong) 1U;
        ///
        @property ulong exclude_hv() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 64U) >> 6U;
                return cast(ulong) result;
        }
        ///
        @property void exclude_hv(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclude_hv_min,
                                "Value is smaller than the minimum value of bitfield 'exclude_hv'");
                assert(v <= exclude_hv_max,
                                "Value is greater than the maximum value of bitfield 'exclude_hv'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 64U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 6U) & 64U));
        }

        enum ulong exclude_hv_min = cast(ulong) 0U;
        enum ulong exclude_hv_max = cast(ulong) 1U;
        ///
        @property ulong exclude_idle() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 128U) >> 7U;
                return cast(ulong) result;
        }
        ///
        @property void exclude_idle(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclude_idle_min,
                                "Value is smaller than the minimum value of bitfield 'exclude_idle'");
                assert(v <= exclude_idle_max,
                                "Value is greater than the maximum value of bitfield 'exclude_idle'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 128U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 7U) & 128U));
        }

        enum ulong exclude_idle_min = cast(ulong) 0U;
        enum ulong exclude_idle_max = cast(ulong) 1U;
        ///
        @property ulong mmap() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 256U) >> 8U;
                return cast(ulong) result;
        }
        ///
        @property void mmap(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= mmap_min, "Value is smaller than the minimum value of bitfield 'mmap'");
                assert(v <= mmap_max, "Value is greater than the maximum value of bitfield 'mmap'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 256U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 8U) & 256U));
        }

        enum ulong mmap_min = cast(ulong) 0U;
        enum ulong mmap_max = cast(ulong) 1U;
        ///
        @property ulong comm() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 512U) >> 9U;
                return cast(ulong) result;
        }
        ///
        @property void comm(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= comm_min, "Value is smaller than the minimum value of bitfield 'comm'");
                assert(v <= comm_max, "Value is greater than the maximum value of bitfield 'comm'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 512U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 9U) & 512U));
        }

        enum ulong comm_min = cast(ulong) 0U;
        enum ulong comm_max = cast(ulong) 1U;
        ///
        @property ulong freq() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 1024U) >> 10U;
                return cast(ulong) result;
        }
        ///
        @property void freq(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= freq_min, "Value is smaller than the minimum value of bitfield 'freq'");
                assert(v <= freq_max, "Value is greater than the maximum value of bitfield 'freq'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 1024U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 10U) & 1024U));
        }

        enum ulong freq_min = cast(ulong) 0U;
        enum ulong freq_max = cast(ulong) 1U;
        ///
        @property ulong inherit_stat() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 2048U) >> 11U;
                return cast(ulong) result;
        }
        ///
        @property void inherit_stat(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= inherit_stat_min,
                                "Value is smaller than the minimum value of bitfield 'inherit_stat'");
                assert(v <= inherit_stat_max,
                                "Value is greater than the maximum value of bitfield 'inherit_stat'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 2048U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 11U) & 2048U));
        }

        enum ulong inherit_stat_min = cast(ulong) 0U;
        enum ulong inherit_stat_max = cast(ulong) 1U;
        ///
        @property ulong enable_on_exec() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 4096U) >> 12U;
                return cast(ulong) result;
        }
        ///
        @property void enable_on_exec(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= enable_on_exec_min,
                                "Value is smaller than the minimum value of bitfield 'enable_on_exec'");
                assert(v <= enable_on_exec_max,
                                "Value is greater than the maximum value of bitfield 'enable_on_exec'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 4096U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 12U) & 4096U));
        }

        enum ulong enable_on_exec_min = cast(ulong) 0U;
        enum ulong enable_on_exec_max = cast(ulong) 1U;
        ///
        @property ulong task() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 8192U) >> 13U;
                return cast(ulong) result;
        }
        ///
        @property void task(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= task_min, "Value is smaller than the minimum value of bitfield 'task'");
                assert(v <= task_max, "Value is greater than the maximum value of bitfield 'task'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 8192U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 13U) & 8192U));
        }

        enum ulong task_min = cast(ulong) 0U;
        enum ulong task_max = cast(ulong) 1U;
        ///
        @property ulong watermark() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 16384U) >> 14U;
                return cast(ulong) result;
        }
        ///
        @property void watermark(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= watermark_min,
                                "Value is smaller than the minimum value of bitfield 'watermark'");
                assert(v <= watermark_max,
                                "Value is greater than the maximum value of bitfield 'watermark'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 16384U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 14U) & 16384U));
        }

        enum ulong watermark_min = cast(ulong) 0U;
        enum ulong watermark_max = cast(ulong) 1U;
        ///
        @property ulong precise_ip() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 98304U) >> 15U;
                return cast(ulong) result;
        }
        ///
        @property void precise_ip(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= precise_ip_min,
                                "Value is smaller than the minimum value of bitfield 'precise_ip'");
                assert(v <= precise_ip_max,
                                "Value is greater than the maximum value of bitfield 'precise_ip'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 98304U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 15U) & 98304U));
        }

        enum ulong precise_ip_min = cast(ulong) 0U;
        enum ulong precise_ip_max = cast(ulong) 3U;
        ///
        @property ulong mmap_data() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 131072U) >> 17U;
                return cast(ulong) result;
        }
        ///
        @property void mmap_data(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= mmap_data_min,
                                "Value is smaller than the minimum value of bitfield 'mmap_data'");
                assert(v <= mmap_data_max,
                                "Value is greater than the maximum value of bitfield 'mmap_data'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 131072U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 17U) & 131072U));
        }

        enum ulong mmap_data_min = cast(ulong) 0U;
        enum ulong mmap_data_max = cast(ulong) 1U;
        ///
        @property ulong sample_id_all() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 262144U) >> 18U;
                return cast(ulong) result;
        }
        ///
        @property void sample_id_all(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= sample_id_all_min,
                                "Value is smaller than the minimum value of bitfield 'sample_id_all'");
                assert(v <= sample_id_all_max,
                                "Value is greater than the maximum value of bitfield 'sample_id_all'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 262144U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 18U) & 262144U));
        }

        enum ulong sample_id_all_min = cast(ulong) 0U;
        enum ulong sample_id_all_max = cast(ulong) 1U;
        ///
        @property ulong exclude_host() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 524288U) >> 19U;
                return cast(ulong) result;
        }
        ///
        @property void exclude_host(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclude_host_min,
                                "Value is smaller than the minimum value of bitfield 'exclude_host'");
                assert(v <= exclude_host_max,
                                "Value is greater than the maximum value of bitfield 'exclude_host'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 524288U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 19U) & 524288U));
        }

        enum ulong exclude_host_min = cast(ulong) 0U;
        enum ulong exclude_host_max = cast(ulong) 1U;
        ///
        @property ulong exclude_guest() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 1048576U) >> 20U;
                return cast(ulong) result;
        }
        ///
        @property void exclude_guest(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclude_guest_min,
                                "Value is smaller than the minimum value of bitfield 'exclude_guest'");
                assert(v <= exclude_guest_max,
                                "Value is greater than the maximum value of bitfield 'exclude_guest'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 1048576U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 20U) & 1048576U));
        }

        enum ulong exclude_guest_min = cast(ulong) 0U;
        enum ulong exclude_guest_max = cast(ulong) 1U;
        ///
        @property ulong exclude_callchain_kernel() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 2097152U) >> 21U;
                return cast(ulong) result;
        }
        ///
        @property void exclude_callchain_kernel(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclude_callchain_kernel_min,
                                "Value is smaller than the minimum value of bitfield 'exclude_callchain_kernel'");
                assert(v <= exclude_callchain_kernel_max,
                                "Value is greater than the maximum value of bitfield 'exclude_callchain_kernel'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 2097152U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 21U) & 2097152U));
        }

        enum ulong exclude_callchain_kernel_min = cast(ulong) 0U;
        enum ulong exclude_callchain_kernel_max = cast(ulong) 1U;
        ///
        @property ulong exclude_callchain_user() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 4194304U) >> 22U;
                return cast(ulong) result;
        }
        ///
        @property void exclude_callchain_user(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= exclude_callchain_user_min,
                                "Value is smaller than the minimum value of bitfield 'exclude_callchain_user'");
                assert(v <= exclude_callchain_user_max,
                                "Value is greater than the maximum value of bitfield 'exclude_callchain_user'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 4194304U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 22U) & 4194304U));
        }

        enum ulong exclude_callchain_user_min = cast(ulong) 0U;
        enum ulong exclude_callchain_user_max = cast(ulong) 1U;
        ///
        @property ulong mmap2() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 8388608U) >> 23U;
                return cast(ulong) result;
        }
        ///
        @property void mmap2(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= mmap2_min,
                                "Value is smaller than the minimum value of bitfield 'mmap2'");
                assert(v <= mmap2_max,
                                "Value is greater than the maximum value of bitfield 'mmap2'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 8388608U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 23U) & 8388608U));
        }

        enum ulong mmap2_min = cast(ulong) 0U;
        enum ulong mmap2_max = cast(ulong) 1U;
        ///
        @property ulong comm_exec() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 16777216U) >> 24U;
                return cast(ulong) result;
        }
        ///
        @property void comm_exec(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= comm_exec_min,
                                "Value is smaller than the minimum value of bitfield 'comm_exec'");
                assert(v <= comm_exec_max,
                                "Value is greater than the maximum value of bitfield 'comm_exec'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 16777216U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 24U) & 16777216U));
        }

        enum ulong comm_exec_min = cast(ulong) 0U;
        enum ulong comm_exec_max = cast(ulong) 1U;
        ///
        @property ulong use_clockid() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 33554432U) >> 25U;
                return cast(ulong) result;
        }
        ///
        @property void use_clockid(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= use_clockid_min,
                                "Value is smaller than the minimum value of bitfield 'use_clockid'");
                assert(v <= use_clockid_max,
                                "Value is greater than the maximum value of bitfield 'use_clockid'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 33554432U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 25U) & 33554432U));
        }

        enum ulong use_clockid_min = cast(ulong) 0U;
        enum ulong use_clockid_max = cast(ulong) 1U;
        ///
        @property ulong context_switch() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 67108864U) >> 26U;
                return cast(ulong) result;
        }
        ///
        @property void context_switch(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= context_switch_min,
                                "Value is smaller than the minimum value of bitfield 'context_switch'");
                assert(v <= context_switch_max,
                                "Value is greater than the maximum value of bitfield 'context_switch'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 67108864U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 26U) & 67108864U));
        }

        enum ulong context_switch_min = cast(ulong) 0U;
        enum ulong context_switch_max = cast(ulong) 1U;
        ///
        @property ulong write_backward() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 134217728U) >> 27U;
                return cast(ulong) result;
        }
        ///
        @property void write_backward(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= write_backward_min,
                                "Value is smaller than the minimum value of bitfield 'write_backward'");
                assert(v <= write_backward_max,
                                "Value is greater than the maximum value of bitfield 'write_backward'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 134217728U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 27U) & 134217728U));
        }

        enum ulong write_backward_min = cast(ulong) 0U;
        enum ulong write_backward_max = cast(ulong) 1U;
        ///
        @property ulong namespaces() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 268435456U) >> 28U;
                return cast(ulong) result;
        }
        ///
        @property void namespaces(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= namespaces_min,
                                "Value is smaller than the minimum value of bitfield 'namespaces'");
                assert(v <= namespaces_max,
                                "Value is greater than the maximum value of bitfield 'namespaces'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(typeof(perf_event_attr_bitmanip)) 268435456U)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 28U) & 268435456U));
        }

        enum ulong namespaces_min = cast(ulong) 0U;
        enum ulong namespaces_max = cast(ulong) 1U;
        ///
        @property ulong __reserved_1() @safe pure nothrow @nogc const
        {
                auto result = (perf_event_attr_bitmanip & 18446744073172680704UL) >> 29U;
                return cast(ulong) result;
        }
        ///
        @property void __reserved_1(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= __reserved_1_min,
                                "Value is smaller than the minimum value of bitfield '__reserved_1'");
                assert(v <= __reserved_1_max,
                                "Value is greater than the maximum value of bitfield '__reserved_1'");
                perf_event_attr_bitmanip = cast(typeof(perf_event_attr_bitmanip))(
                                (perf_event_attr_bitmanip & (-1 - cast(
                                typeof(perf_event_attr_bitmanip)) 18446744073172680704UL)) | (
                                (cast(typeof(perf_event_attr_bitmanip)) v << 29U) & 18446744073172680704UL));
        }

        enum ulong __reserved_1_min = cast(ulong) 0U;
        enum ulong __reserved_1_max = cast(ulong) 34359738367UL;
        ///
        union
        {
                uint wakeup_events; /** wakeup every n events */
                uint wakeup_watermark; /** bytes before wakeup   */
        }
        ///
        uint bp_type;

        union
        {
                ///
                ulong bp_addr;
                ulong config1; /** extension of config */
        }

        union
        {
                ///
                ulong bp_len;
                ulong config2; /** extension of config1 */
        }

        ulong branch_sample_type; /** enum perf_branch_sample_type */

        /**
        * Defines set of user regs to dump on samples.
        * See asm/perf_regs.h for details.
        */
        ulong sample_regs_user;

        /**
        * Defines size of the user stack to dump on samples.
        */
        uint sample_stack_user;
        ///
        int clockid;

        /**
        * Defines set of regs to dump for each sample
        * state captured on:
        *  - precise = 0: PMU interrupt
        *  - precise > 0: sampled instruction
        *
        * See asm/perf_regs.h for details.
        */
        ulong sample_regs_intr;

        /**
        * Wakeup watermark for AUX area
        */
        uint aux_watermark;
        ///
        ushort sample_max_stack;
        /** align to __u64 */
        ushort __reserved_2;
}
///
extern (D) auto perf_flags(T)(auto ref T attr)
{
        return *(&attr.read_format + 1);
}

/**
 * Ioctls that can be done on a perf event fd:
 */
enum PERF_EVENT_IOC_ENABLE = _IO('$', 0);
///
enum PERF_EVENT_IOC_DISABLE = _IO('$', 1);
///
enum PERF_EVENT_IOC_REFRESH = _IO('$', 2);
///
enum PERF_EVENT_IOC_RESET = _IO('$', 3);
///
enum PERF_EVENT_IOC_PERIOD = _IOW!ulong('$', 4);
///
enum PERF_EVENT_IOC_SET_OUTPUT = _IO('$', 5);
///
enum PERF_EVENT_IOC_SET_FILTER = _IOW!(char*)('$', 6);
///
enum PERF_EVENT_IOC_ID = _IOR!(ulong*)('$', 7);
///
enum PERF_EVENT_IOC_SET_BPF = _IOW!uint('$', 8);
///
enum PERF_EVENT_IOC_PAUSE_OUTPUT = _IOW!uint('$', 9);

///
enum perf_event_ioc_flags
{
        PERF_IOC_FLAG_GROUP = 1U << 0
}

/**
 * Structure of the page that can be mapped via mmap
 */
struct perf_event_mmap_page
{
        uint version_; /** version number of this structure */
        uint compat_version; /** lowest version this is compat with */

        /**
        * Bits needed to read the hw events in user-space.
        *   ---
        *   u32 seq, time_mult, time_shift, index, width;
        *   u64 count, enabled, running;
        *   u64 cyc, time_offset;
        *   s64 pmc = 0;
        *
        *   do {
        *     seq = pc->lock;
        *     barrier()
        *
        *     enabled = pc->time_enabled;
        *     running = pc->time_running;
        *
        *     if (pc->cap_usr_time && enabled != running) {
        *       cyc = rdtsc();
        *       time_offset = pc->time_offset;
        *       time_mult   = pc->time_mult;
        *       time_shift  = pc->time_shift;
        *     }
        *
        *     index = pc->index;
        *     count = pc->offset;
        *     if (pc->cap_user_rdpmc && index) {
        *       width = pc->pmc_width;
        *       pmc = rdpmc(index - 1);
        *     }
        *
        *     barrier();
        *   } while (pc->lock != seq);
        *   ---
        * NOTE: for obvious reason this only works on self-monitoring
        *       processes.
        */
        uint lock; /** seqlock for synchronization */
        uint index; /** hardware event identifier */
        long offset; /** add to hardware event value */
        ulong time_enabled; /** time event active */
        ulong time_running; /** time event on cpu */
        ///
        union
        {
                ///
                ulong capabilities;

                struct
                {
                        /*  mixin(bitfields!(ulong, "cap_bit0", 1, ulong, "cap_bit0_is_deprecated", 1, ulong,
                    "cap_user_rdpmc", 1, ulong, "cap_user_time", 1, ulong,
                    "cap_user_time_zero", 1, ulong, "cap_____res", 59)); */

                        private ulong mmap_page_bitmanip;
                        ///
                        @property ulong cap_bit0() @safe pure nothrow @nogc const
                        {
                                auto result = (mmap_page_bitmanip & 1U) >> 0U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void cap_bit0(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= cap_bit0_min,
                                                "Value is smaller than the minimum value of bitfield 'cap_bit0'");
                                assert(v <= cap_bit0_max,
                                                "Value is greater than the maximum value of bitfield 'cap_bit0'");
                                mmap_page_bitmanip = cast(typeof(mmap_page_bitmanip))(
                                                (mmap_page_bitmanip & (-1 - cast(typeof(mmap_page_bitmanip)) 1U)) | (
                                                (cast(typeof(mmap_page_bitmanip)) v << 0U) & 1U));
                        }

                        enum ulong cap_bit0_min = cast(ulong) 0U;
                        enum ulong cap_bit0_max = cast(ulong) 1U;
                        ///
                        @property ulong cap_bit0_is_deprecated() @safe pure nothrow @nogc const
                        {
                                auto result = (mmap_page_bitmanip & 2U) >> 1U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void cap_bit0_is_deprecated(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= cap_bit0_is_deprecated_min,
                                                "Value is smaller than the minimum value of bitfield 'cap_bit0_is_deprecated'");
                                assert(v <= cap_bit0_is_deprecated_max,
                                                "Value is greater than the maximum value of bitfield 'cap_bit0_is_deprecated'");
                                mmap_page_bitmanip = cast(typeof(mmap_page_bitmanip))(
                                                (mmap_page_bitmanip & (-1 - cast(typeof(mmap_page_bitmanip)) 2U)) | (
                                                (cast(typeof(mmap_page_bitmanip)) v << 1U) & 2U));
                        }

                        enum ulong cap_bit0_is_deprecated_min = cast(ulong) 0U;
                        enum ulong cap_bit0_is_deprecated_max = cast(ulong) 1U;
                        ///
                        @property ulong cap_user_rdpmc() @safe pure nothrow @nogc const
                        {
                                auto result = (mmap_page_bitmanip & 4U) >> 2U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void cap_user_rdpmc(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= cap_user_rdpmc_min,
                                                "Value is smaller than the minimum value of bitfield 'cap_user_rdpmc'");
                                assert(v <= cap_user_rdpmc_max,
                                                "Value is greater than the maximum value of bitfield 'cap_user_rdpmc'");
                                mmap_page_bitmanip = cast(typeof(mmap_page_bitmanip))(
                                                (mmap_page_bitmanip & (-1 - cast(typeof(mmap_page_bitmanip)) 4U)) | (
                                                (cast(typeof(mmap_page_bitmanip)) v << 2U) & 4U));
                        }

                        enum ulong cap_user_rdpmc_min = cast(ulong) 0U;
                        enum ulong cap_user_rdpmc_max = cast(ulong) 1U;
                        ///
                        @property ulong cap_user_time() @safe pure nothrow @nogc const
                        {
                                auto result = (mmap_page_bitmanip & 8U) >> 3U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void cap_user_time(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= cap_user_time_min,
                                                "Value is smaller than the minimum value of bitfield 'cap_user_time'");
                                assert(v <= cap_user_time_max,
                                                "Value is greater than the maximum value of bitfield 'cap_user_time'");
                                mmap_page_bitmanip = cast(typeof(mmap_page_bitmanip))(
                                                (mmap_page_bitmanip & (-1 - cast(typeof(mmap_page_bitmanip)) 8U)) | (
                                                (cast(typeof(mmap_page_bitmanip)) v << 3U) & 8U));
                        }

                        enum ulong cap_user_time_min = cast(ulong) 0U;
                        enum ulong cap_user_time_max = cast(ulong) 1U;
                        ///
                        @property ulong cap_user_time_zero() @safe pure nothrow @nogc const
                        {
                                auto result = (mmap_page_bitmanip & 16U) >> 4U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void cap_user_time_zero(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= cap_user_time_zero_min,
                                                "Value is smaller than the minimum value of bitfield 'cap_user_time_zero'");
                                assert(v <= cap_user_time_zero_max,
                                                "Value is greater than the maximum value of bitfield 'cap_user_time_zero'");
                                mmap_page_bitmanip = cast(typeof(mmap_page_bitmanip))(
                                                (mmap_page_bitmanip & (-1 - cast(typeof(mmap_page_bitmanip)) 16U)) | (
                                                (cast(typeof(mmap_page_bitmanip)) v << 4U) & 16U));
                        }

                        enum ulong cap_user_time_zero_min = cast(ulong) 0U;
                        enum ulong cap_user_time_zero_max = cast(ulong) 1U;
                        ///
                        @property ulong cap_____res() @safe pure nothrow @nogc const
                        {
                                auto result = (mmap_page_bitmanip & 18446744073709551584UL) >> 5U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void cap_____res(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= cap_____res_min,
                                                "Value is smaller than the minimum value of bitfield 'cap_____res'");
                                assert(v <= cap_____res_max,
                                                "Value is greater than the maximum value of bitfield 'cap_____res'");
                                mmap_page_bitmanip = cast(typeof(mmap_page_bitmanip))((mmap_page_bitmanip & (
                                                -1 - cast(typeof(mmap_page_bitmanip)) 18446744073709551584UL)) | (
                                                (cast(typeof(mmap_page_bitmanip)) v << 5U) & 18446744073709551584UL));
                        }

                        enum ulong cap_____res_min = cast(ulong) 0U;
                        enum ulong cap_____res_max = cast(ulong) 576460752303423487UL;
                }
        }

        /**
        * If cap_user_rdpmc this field provides the bit-width of the value
        * read using the rdpmc() or equivalent instruction. This can be used
        * to sign extend the result like:
        *
        *   pmc <<= 64 - width;
        *   pmc >>= 64 - width; // signed shift right
        *   count += pmc;
        */
        ushort pmc_width;

        /**
        * If cap_usr_time the below fields can be used to compute the time
        * delta since time_enabled (in ns) using rdtsc or similar.
        *
        *   u64 quot, rem;
        *   u64 delta;
        *
        *   quot = (cyc >> time_shift);
        *   rem = cyc & (((u64)1 << time_shift) - 1);
        *   delta = time_offset + quot * time_mult +
        *              ((rem * time_mult) >> time_shift);
        *
        * Where time_offset,time_mult,time_shift and cyc are read in the
        * seqcount loop described above. This delta can then be added to
        * enabled and possible running (if index), improving the scaling:
        *
        *   enabled += delta;
        *   if (index)
        *     running += delta;
        *
        *   quot = count / running;
        *   rem  = count % running;
        *   count = quot * enabled + (rem * enabled) / running;
        */
        ushort time_shift;
        ///
        uint time_mult;
        ///
        ulong time_offset;
        /**
        * If cap_usr_time_zero, the hardware clock (e.g. TSC) can be calculated
        * from sample timestamps.
        *
        *   time = timestamp - time_zero;
        *   quot = time / time_mult;
        *   rem  = time % time_mult;
        *   cyc = (quot << time_shift) + (rem << time_shift) / time_mult;
        *
        * And vice versa:
        *
        *   quot = cyc >> time_shift;
        *   rem  = cyc & (((u64)1 << time_shift) - 1);
        *   timestamp = time_zero + quot * time_mult +
        *               ((rem * time_mult) >> time_shift);
        */
        ulong time_zero;
        uint size; /** Header size up to __reserved[] fields. */

        /**
        * Hole for extension of the self monitor capabilities
        */

        ubyte[948] __reserved; /** align to 1k. */

        /**
        * Control data for the mmap() data buffer.
        *
        * User-space reading the @data_head value should issue an smp_rmb(),
        * after reading this value.
        *
        * When the mapping is PROT_WRITE the @data_tail value should be
        * written by userspace to reflect the last read data, after issueing
        * an smp_mb() to separate the data read from the ->data_tail store.
        * In this case the kernel will not over-write unread data.
        *
        * See perf_output_put_handle() for the data ordering.
        *
        * data_{offset,size} indicate the location and size of the perf record
        * buffer within the mmapped area.
        */
        ulong data_head; /** head in the data section */
        ulong data_tail; /** user-space written tail */
        ulong data_offset; /** where the buffer starts */
        ulong data_size; /** data buffer size */

        /**
        * AUX area is defined by aux_{offset,size} fields that should be set
        * by the userspace, so that
        *   ---
        *   aux_offset >= data_offset + data_size
        *   ---
        * prior to mmap()ing it. Size of the mmap()ed area should be aux_size.
        *
        * Ring buffer pointers aux_{head,tail} have the same semantics as
        * data_{head,tail} and same ordering rules apply.
        */
        ulong aux_head;
        ///
        ulong aux_tail;
        ///
        ulong aux_offset;
        ///
        ulong aux_size;
}
///
enum PERF_RECORD_MISC_CPUMODE_MASK = 7 << 0;
///
enum PERF_RECORD_MISC_CPUMODE_UNKNOWN = 0 << 0;
///
enum PERF_RECORD_MISC_KERNEL = 1 << 0;
///
enum PERF_RECORD_MISC_USER = 2 << 0;
///
enum PERF_RECORD_MISC_HYPERVISOR = 3 << 0;
///
enum PERF_RECORD_MISC_GUEST_KERNEL = 4 << 0;
///
enum PERF_RECORD_MISC_GUEST_USER = 5 << 0;

/**
 * Indicates that /proc/PID/maps parsing are truncated by time out.
 */
enum PERF_RECORD_MISC_PROC_MAP_PARSE_TIMEOUT = 1 << 12;
/**
 * PERF_RECORD_MISC_MMAP_DATA and PERF_RECORD_MISC_COMM_EXEC are used on
 * different events so can reuse the same bit position.
 * Ditto PERF_RECORD_MISC_SWITCH_OUT.
 */
enum PERF_RECORD_MISC_MMAP_DATA = 1 << 13;
///
enum PERF_RECORD_MISC_COMM_EXEC = 1 << 13;
///
enum PERF_RECORD_MISC_SWITCH_OUT = 1 << 13;
/**
 * Indicates that the content of PERF_SAMPLE_IP points to
 * the actual instruction that triggered the event. See also
 * perf_event_attr::precise_ip.
 */
enum PERF_RECORD_MISC_EXACT_IP = 1 << 14;
/**
 * Reserve the last bit to indicate some extended misc field
 */
enum PERF_RECORD_MISC_EXT_RESERVED = 1 << 15;
///
struct perf_event_header
{
        ///
        uint type;
        ///
        ushort misc;
        ///
        ushort size;
}
///
struct perf_ns_link_info
{
        ///
        ulong dev;
        ///
        ulong ino;
}

enum
{
        ///
        NET_NS_INDEX = 0,
        ///
        UTS_NS_INDEX = 1,
        ///
        IPC_NS_INDEX = 2,
        ///
        PID_NS_INDEX = 3,
        ///
        USER_NS_INDEX = 4,
        ///
        MNT_NS_INDEX = 5,
        ///
        CGROUP_NS_INDEX = 6,
        NR_NAMESPACES = 7 /** number of available namespaces */
}
///
enum perf_event_type
{
        /**
        * If perf_event_attr.sample_id_all is set then all event types will
        * have the sample_type selected fields related to where/when
        * (identity) an event took place (TID, TIME, ID, STREAM_ID, CPU,
        * IDENTIFIER) described in PERF_RECORD_SAMPLE below, it will be stashed
        * just after the perf_event_header and the fields already present for
        * the existing fields, i.e. at the end of the payload. That way a newer
        * perf.data file will be supported by older perf tools, with these new
        * optional fields being ignored.
        * ---
        * struct sample_id {
        *     { u32            pid, tid; } && PERF_SAMPLE_TID
        *     { u64            time;     } && PERF_SAMPLE_TIME
        *     { u64            id;       } && PERF_SAMPLE_ID
        *     { u64            stream_id;} && PERF_SAMPLE_STREAM_ID
        *     { u32            cpu, res; } && PERF_SAMPLE_CPU
        *    { u64            id;      } && PERF_SAMPLE_IDENTIFIER
        * } && perf_event_attr::sample_id_all
        * ---
        * Note that PERF_SAMPLE_IDENTIFIER duplicates PERF_SAMPLE_ID.  The
        * advantage of PERF_SAMPLE_IDENTIFIER is that its position is fixed
        * relative to header.size.
        */

        /*
        * The MMAP events record the PROT_EXEC mappings so that we can
        * correlate userspace IPs to code. They have the following structure:
        * ---
        * struct {
        *    struct perf_event_header    header;
        *
        *    u32                pid, tid;
        *    u64                addr;
        *    u64                len;
        *    u64                pgoff;
        *    char                filename[];
        *     struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_MMAP = 1,

        /**
        * ---
        * struct {
        *    struct perf_event_header    header;
        *    u64                id;
        *    u64                lost;
        *     struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_LOST = 2,

        /**
        * ---
        * struct {
        *    struct perf_event_header    header;
        *
        *    u32                pid, tid;
        *    char                comm[];
        *     struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_COMM = 3,

        /**
        * ---
        * struct {
        *    struct perf_event_header    header;
        *    u32                pid, ppid;
        *    u32                tid, ptid;
        *    u64                time;
        *     struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_EXIT = 4,

        /**
        * ---
        * struct {
        *    struct perf_event_header    header;
        *    u64                time;
        *    u64                id;
        *    u64                stream_id;
        *    struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_THROTTLE = 5,
        PERF_RECORD_UNTHROTTLE = 6,
        /**
        * ---
        * struct {
        *    struct perf_event_header    header;
        *    u32                pid, ppid;
        *    u32                tid, ptid;
        *    u64                time;
        *     struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_FORK = 7,
        /**
        * ---
        * struct {
        *    struct perf_event_header    header;
        *    u32                pid, tid;
        *
        *    struct read_format        values;
        *     struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_READ = 8,
        /**
        * ---
        * struct {
        *    struct perf_event_header    header;
        *
        *    #
        *    # Note that PERF_SAMPLE_IDENTIFIER duplicates PERF_SAMPLE_ID.
        *    # The advantage of PERF_SAMPLE_IDENTIFIER is that its position
        *    # is fixed relative to header.
        *    #
        *
        *    { u64            id;      } && PERF_SAMPLE_IDENTIFIER
        *    { u64            ip;      } && PERF_SAMPLE_IP
        *    { u32            pid, tid; } && PERF_SAMPLE_TID
        *    { u64            time;     } && PERF_SAMPLE_TIME
        *    { u64            addr;     } && PERF_SAMPLE_ADDR
        *    { u64            id;      } && PERF_SAMPLE_ID
        *    { u64            stream_id;} && PERF_SAMPLE_STREAM_ID
        *    { u32            cpu, res; } && PERF_SAMPLE_CPU
        *    { u64            period;   } && PERF_SAMPLE_PERIOD
        *
        *    { struct read_format    values;      } && PERF_SAMPLE_READ
        *
        *    { u64            nr,
        *      u64            ips[nr];  } && PERF_SAMPLE_CALLCHAIN
        *
        *    #
        *    # The RAW record below is opaque data wrt the ABI
        *    #
        *    # That is, the ABI doesn't make any promises wrt to
        *    # the stability of its content, it may vary depending
        *    # on event, hardware, kernel version and phase of
        *    # the moon.
        *    #
        *    # In other words, PERF_SAMPLE_RAW contents are not an ABI.
        *    #
        *
        *    { u32            size;
        *      char                  data[size];}&& PERF_SAMPLE_RAW
        *
        *    { u64                   nr;
        *        { u64 from, to, flags } lbr[nr];} && PERF_SAMPLE_BRANCH_STACK
        *
        *     { u64            abi; # enum perf_sample_regs_abi
        *       u64            regs[weight(mask)]; } && PERF_SAMPLE_REGS_USER
        *
        *     { u64            size;
        *       char            data[size];
        *       u64            dyn_size; } && PERF_SAMPLE_STACK_USER
        *
        *    { u64            weight;   } && PERF_SAMPLE_WEIGHT
        *    { u64            data_src; } && PERF_SAMPLE_DATA_SRC
        *    { u64            transaction; } && PERF_SAMPLE_TRANSACTION
        *    { u64            abi; # enum perf_sample_regs_abi
        *      u64            regs[weight(mask)]; } && PERF_SAMPLE_REGS_INTR
        *    { u64            phys_addr;} && PERF_SAMPLE_PHYS_ADDR
        * };
        * ---
        */
        PERF_RECORD_SAMPLE = 9,

        /**
        * ---
        * The MMAP2 records are an augmented version of MMAP, they add
        * maj, min, ino numbers to be used to uniquely identify each mapping
        *
        * struct {
        *    struct perf_event_header    header;
        *
        *    u32                pid, tid;
        *    u64                addr;
        *    u64                len;
        *    u64                pgoff;
        *    u32                maj;
        *    u32                min;
        *    u64                ino;
        *    u64                ino_generation;
        *    u32                prot, flags;
        *    char                filename[];
        *     struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_MMAP2 = 10,

        /**
        * Records that new data landed in the AUX buffer part.
        * ---
        * struct {
        *     struct perf_event_header    header;
        *
        *     u64                aux_offset;
        *     u64                aux_size;
        *    u64                flags;
        *     struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_AUX = 11,

        /**
        * ---
        * Indicates that instruction trace has started
        *
        * struct {
        *    struct perf_event_header    header;
        *    u32                pid;
        *    u32                tid;
        * };
        * ---
        */
        PERF_RECORD_ITRACE_START = 12,

        /**
        * Records the dropped/lost sample number.
        * ---
        * struct {
        *    struct perf_event_header    header;
        *
        *    u64                lost;
        *    struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_LOST_SAMPLES = 13,

        /**
        *
        * Records a context switch in or out (flagged by
        * PERF_RECORD_MISC_SWITCH_OUT). See also
        * PERF_RECORD_SWITCH_CPU_WIDE.
        * ---
        * struct {
        *    struct perf_event_header    header;
        *    struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_SWITCH = 14,

        /**
        * CPU-wide version of PERF_RECORD_SWITCH with next_prev_pid and
        * next_prev_tid that are the next (switching out) or previous
        * (switching in) pid/tid.
        *  ---
        * struct {
        *    struct perf_event_header    header;
        *    u32                next_prev_pid;
        *    u32                next_prev_tid;
        *    struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_SWITCH_CPU_WIDE = 15,

        /**
        * ---
        * struct {
        *    struct perf_event_header    header;
        *    u32                pid;
        *    u32                tid;
        *    u64                nr_namespaces;
        *    { u64                dev, inode; } [nr_namespaces];
        *    struct sample_id        sample_id;
        * };
        * ---
        */
        PERF_RECORD_NAMESPACES = 16,

        PERF_RECORD_MAX = 17 /* non-ABI */
}
///
enum PERF_MAX_STACK_DEPTH = 127;
///
enum PERF_MAX_CONTEXTS_PER_STACK = 8;
///
enum perf_callchain_context
{
        ///
        PERF_CONTEXT_HV = cast(ulong)-32,
        ///
        PERF_CONTEXT_KERNEL = cast(ulong)-128,
        ///
        PERF_CONTEXT_USER = cast(ulong)-512,
        ///
        PERF_CONTEXT_GUEST = cast(ulong)-2048,
        ///
        PERF_CONTEXT_GUEST_KERNEL = cast(ulong)-2176,
        ///
        PERF_CONTEXT_GUEST_USER = cast(ulong)-2560,
        ///
        PERF_CONTEXT_MAX = cast(ulong)-4095
}

/**
 * PERF_RECORD_AUX::flags bits
 */
enum PERF_AUX_FLAG_TRUNCATED = 0x01; /** record was truncated to fit */
enum PERF_AUX_FLAG_OVERWRITE = 0x02; /** snapshot from overwrite mode */
enum PERF_AUX_FLAG_PARTIAL = 0x04; /** record contains gaps */
enum PERF_AUX_FLAG_COLLISION = 0x08; /** sample collided with another */
///
enum PERF_FLAG_FD_NO_GROUP = 1UL << 0;
///
enum PERF_FLAG_FD_OUTPUT = 1UL << 1;
enum PERF_FLAG_PID_CGROUP = 1UL << 2; /** pid=cgroup id, per-cpu mode only */
enum PERF_FLAG_FD_CLOEXEC = 1UL << 3; /** O_CLOEXEC */
///perm_mem_data_src is endian specific.
version (LittleEndian)
{
        ///
        union perf_mem_data_src
        {
                ///
                ulong val;

                struct
                {
                        /* mixin(bitfields!(ulong, "mem_op", 5, ulong, "mem_lvl", 14, ulong,
                    "mem_snoop", 5, ulong, "mem_lock", 2, ulong, "mem_dtlb", 7, ulong,
                    "mem_lvl_num", 4, ulong, "mem_remote", 1, ulong,
                    "mem_snoopx", 2, ulong, "mem_rsvd", 24)); */

                        private ulong perf_mem_data_src_bitmanip;
                        ///
                        @property ulong mem_op() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 31U) >> 0U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_op(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_op_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_op'");
                                assert(v <= mem_op_max,
                                                "Value is greater than the maximum value of bitfield 'mem_op'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))((perf_mem_data_src_bitmanip & (
                                                -1 - cast(typeof(perf_mem_data_src_bitmanip)) 31U)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 0U) & 31U));
                        }

                        enum ulong mem_op_min = cast(ulong) 0U;
                        enum ulong mem_op_max = cast(ulong) 31U;
                        ///
                        @property ulong mem_lvl() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 524256U) >> 5U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_lvl(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_lvl_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_lvl'");
                                assert(v <= mem_lvl_max,
                                                "Value is greater than the maximum value of bitfield 'mem_lvl'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))((perf_mem_data_src_bitmanip & (
                                                -1 - cast(typeof(perf_mem_data_src_bitmanip)) 524256U)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 5U) & 524256U));
                        }

                        enum ulong mem_lvl_min = cast(ulong) 0U;
                        enum ulong mem_lvl_max = cast(ulong) 16383U;
                        ///
                        @property ulong mem_snoop() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 16252928U) >> 19U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_snoop(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_snoop_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_snoop'");
                                assert(v <= mem_snoop_max,
                                                "Value is greater than the maximum value of bitfield 'mem_snoop'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))((perf_mem_data_src_bitmanip & (
                                                -1 - cast(typeof(perf_mem_data_src_bitmanip)) 16252928U)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 19U) & 16252928U));
                        }

                        enum ulong mem_snoop_min = cast(ulong) 0U;
                        enum ulong mem_snoop_max = cast(ulong) 31U;
                        ///
                        @property ulong mem_lock() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 50331648U) >> 24U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_lock(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_lock_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_lock'");
                                assert(v <= mem_lock_max,
                                                "Value is greater than the maximum value of bitfield 'mem_lock'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))((perf_mem_data_src_bitmanip & (
                                                -1 - cast(typeof(perf_mem_data_src_bitmanip)) 50331648U)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 24U) & 50331648U));
                        }

                        enum ulong mem_lock_min = cast(ulong) 0U;
                        enum ulong mem_lock_max = cast(ulong) 3U;
                        ///
                        @property ulong mem_dtlb() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 8522825728UL) >> 26U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_dtlb(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_dtlb_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_dtlb'");
                                assert(v <= mem_dtlb_max,
                                                "Value is greater than the maximum value of bitfield 'mem_dtlb'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))((perf_mem_data_src_bitmanip & (
                                                -1 - cast(typeof(perf_mem_data_src_bitmanip)) 8522825728UL)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 26U) & 8522825728UL));
                        }

                        enum ulong mem_dtlb_min = cast(ulong) 0U;
                        enum ulong mem_dtlb_max = cast(ulong) 127U;
                        ///
                        @property ulong mem_lvl_num() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 128849018880UL) >> 33U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_lvl_num(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_lvl_num_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_lvl_num'");
                                assert(v <= mem_lvl_num_max,
                                                "Value is greater than the maximum value of bitfield 'mem_lvl_num'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))((perf_mem_data_src_bitmanip & (
                                                -1 - cast(typeof(perf_mem_data_src_bitmanip)) 128849018880UL)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 33U) & 128849018880UL));
                        }

                        enum ulong mem_lvl_num_min = cast(ulong) 0U;
                        enum ulong mem_lvl_num_max = cast(ulong) 15U;
                        ///
                        @property ulong mem_remote() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 137438953472UL) >> 37U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_remote(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_remote_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_remote'");
                                assert(v <= mem_remote_max,
                                                "Value is greater than the maximum value of bitfield 'mem_remote'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))((perf_mem_data_src_bitmanip & (
                                                -1 - cast(typeof(perf_mem_data_src_bitmanip)) 137438953472UL)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 37U) & 137438953472UL));
                        }

                        enum ulong mem_remote_min = cast(ulong) 0U;
                        enum ulong mem_remote_max = cast(ulong) 1U;
                        ///
                        @property ulong mem_snoopx() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 824633720832UL) >> 38U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_snoopx(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_snoopx_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_snoopx'");
                                assert(v <= mem_snoopx_max,
                                                "Value is greater than the maximum value of bitfield 'mem_snoopx'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))((perf_mem_data_src_bitmanip & (
                                                -1 - cast(typeof(perf_mem_data_src_bitmanip)) 824633720832UL)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 38U) & 824633720832UL));
                        }

                        enum ulong mem_snoopx_min = cast(ulong) 0U;
                        enum ulong mem_snoopx_max = cast(ulong) 3U;
                        ///
                        @property ulong mem_rsvd() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src_bitmanip & 18446742974197923840UL) >> 40U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_rsvd(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_rsvd_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_rsvd'");
                                assert(v <= mem_rsvd_max,
                                                "Value is greater than the maximum value of bitfield 'mem_rsvd'");
                                perf_mem_data_src_bitmanip = cast(
                                                typeof(perf_mem_data_src_bitmanip))(
                                                (perf_mem_data_src_bitmanip & (-1 - cast(
                                                typeof(perf_mem_data_src_bitmanip)) 18446742974197923840UL)) | (
                                                (cast(typeof(perf_mem_data_src_bitmanip)) v << 40U) & 18446742974197923840UL));
                        }

                        enum ulong mem_rsvd_min = cast(ulong) 0U;
                        enum ulong mem_rsvd_max = cast(ulong) 16777215U;

                }
        }
}
else
{
        ///
        union perf_mem_data_src
        {
                ///
                ulong val;

                struct
                {
                        /* mixin(bitfields!(ulong, "mem_rsvd", 24, ulong, "mem_snoopx", 2, ulong,
                    "mem_remote", 1, ulong, "mem_lvl_num", 4, ulong, "mem_dtlb", 7, ulong,
                    "mem_lock", 2, ulong, "mem_snoop", 5, ulong, "mem_lvl",
                    14, ulong, "mem_op", 5)); */
                        private ulong perf_mem_data_src;
                        ///
                        @property ulong mem_rsvd() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 16777215U) >> 0U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_rsvd(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_rsvd_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_rsvd'");
                                assert(v <= mem_rsvd_max,
                                                "Value is greater than the maximum value of bitfield 'mem_rsvd'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))(
                                                (perf_mem_data_src & (-1 - cast(typeof(perf_mem_data_src)) 16777215U)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 0U) & 16777215U));
                        }

                        enum ulong mem_rsvd_min = cast(ulong) 0U;
                        enum ulong mem_rsvd_max = cast(ulong) 16777215U;
                        ///
                        @property ulong mem_snoopx() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 50331648U) >> 24U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_snoopx(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_snoopx_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_snoopx'");
                                assert(v <= mem_snoopx_max,
                                                "Value is greater than the maximum value of bitfield 'mem_snoopx'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))(
                                                (perf_mem_data_src & (-1 - cast(typeof(perf_mem_data_src)) 50331648U)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 24U) & 50331648U));
                        }

                        enum ulong mem_snoopx_min = cast(ulong) 0U;
                        enum ulong mem_snoopx_max = cast(ulong) 3U;
                        ///
                        @property ulong mem_remote() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 67108864U) >> 26U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_remote(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_remote_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_remote'");
                                assert(v <= mem_remote_max,
                                                "Value is greater than the maximum value of bitfield 'mem_remote'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))(
                                                (perf_mem_data_src & (-1 - cast(typeof(perf_mem_data_src)) 67108864U)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 26U) & 67108864U));
                        }

                        enum ulong mem_remote_min = cast(ulong) 0U;
                        enum ulong mem_remote_max = cast(ulong) 1U;
                        ///
                        @property ulong mem_lvl_num() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 2013265920U) >> 27U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_lvl_num(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_lvl_num_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_lvl_num'");
                                assert(v <= mem_lvl_num_max,
                                                "Value is greater than the maximum value of bitfield 'mem_lvl_num'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))(
                                                (perf_mem_data_src & (-1 - cast(typeof(perf_mem_data_src)) 2013265920U)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 27U) & 2013265920U));
                        }

                        enum ulong mem_lvl_num_min = cast(ulong) 0U;
                        enum ulong mem_lvl_num_max = cast(ulong) 15U;
                        ///
                        @property ulong mem_dtlb() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 272730423296UL) >> 31U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_dtlb(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_dtlb_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_dtlb'");
                                assert(v <= mem_dtlb_max,
                                                "Value is greater than the maximum value of bitfield 'mem_dtlb'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))(
                                                (perf_mem_data_src & (-1 - cast(typeof(perf_mem_data_src)) 272730423296UL)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 31U) & 272730423296UL));
                        }

                        enum ulong mem_dtlb_min = cast(ulong) 0U;
                        enum ulong mem_dtlb_max = cast(ulong) 127U;
                        ///
                        @property ulong mem_lock() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 824633720832UL) >> 38U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_lock(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_lock_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_lock'");
                                assert(v <= mem_lock_max,
                                                "Value is greater than the maximum value of bitfield 'mem_lock'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))(
                                                (perf_mem_data_src & (-1 - cast(typeof(perf_mem_data_src)) 824633720832UL)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 38U) & 824633720832UL));
                        }

                        enum ulong mem_lock_min = cast(ulong) 0U;
                        enum ulong mem_lock_max = cast(ulong) 3U;
                        ///
                        @property ulong mem_snoop() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 34084860461056UL) >> 40U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_snoop(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_snoop_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_snoop'");
                                assert(v <= mem_snoop_max,
                                                "Value is greater than the maximum value of bitfield 'mem_snoop'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))(
                                                (perf_mem_data_src & (-1 - cast(typeof(perf_mem_data_src)) 34084860461056UL)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 40U) & 34084860461056UL));
                        }

                        enum ulong mem_snoop_min = cast(ulong) 0U;
                        enum ulong mem_snoop_max = cast(ulong) 31U;
                        ///
                        @property ulong mem_lvl() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 576425567931334656UL) >> 45U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_lvl(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_lvl_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_lvl'");
                                assert(v <= mem_lvl_max,
                                                "Value is greater than the maximum value of bitfield 'mem_lvl'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))((perf_mem_data_src & (
                                                -1 - cast(typeof(perf_mem_data_src)) 576425567931334656UL)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 45U) & 576425567931334656UL));
                        }

                        enum ulong mem_lvl_min = cast(ulong) 0U;
                        enum ulong mem_lvl_max = cast(ulong) 16383U;
                        ///
                        @property ulong mem_op() @safe pure nothrow @nogc const
                        {
                                auto result = (perf_mem_data_src & 17870283321406128128UL) >> 59U;
                                return cast(ulong) result;
                        }
                        ///
                        @property void mem_op(ulong v) @safe pure nothrow @nogc
                        {
                                assert(v >= mem_op_min,
                                                "Value is smaller than the minimum value of bitfield 'mem_op'");
                                assert(v <= mem_op_max,
                                                "Value is greater than the maximum value of bitfield 'mem_op'");
                                perf_mem_data_src = cast(typeof(perf_mem_data_src))((perf_mem_data_src & (
                                                -1 - cast(typeof(perf_mem_data_src)) 17870283321406128128UL)) | (
                                                (cast(typeof(perf_mem_data_src)) v << 59U) & 17870283321406128128UL));
                        }

                        enum ulong mem_op_min = cast(ulong) 0U;
                        enum ulong mem_op_max = cast(ulong) 31U;
                }
        }
}

/* snoop mode, ext */
/* remote */
/* memory hierarchy level number */
/* tlb access */
/* lock instr */
/* snoop mode */
/* memory hierarchy level */
/* type of opcode */

/** type of opcode (load/store/prefetch,code) */
enum PERF_MEM_OP_NA = 0x01; /** not available */
enum PERF_MEM_OP_LOAD = 0x02; /** load instruction */
enum PERF_MEM_OP_STORE = 0x04; /** store instruction */
enum PERF_MEM_OP_PFETCH = 0x08; /** prefetch */
enum PERF_MEM_OP_EXEC = 0x10; /** code (execution) */
enum PERF_MEM_OP_SHIFT = 0;

/* memory hierarchy (memory level, hit or miss) */
enum PERF_MEM_LVL_NA = 0x01; /** not available */
enum PERF_MEM_LVL_HIT = 0x02; /** hit level */
enum PERF_MEM_LVL_MISS = 0x04; /** miss level  */
enum PERF_MEM_LVL_L1 = 0x08; /** L1 */
enum PERF_MEM_LVL_LFB = 0x10; /** Line Fill Buffer */
enum PERF_MEM_LVL_L2 = 0x20; /** L2 */
enum PERF_MEM_LVL_L3 = 0x40; /** L3 */
enum PERF_MEM_LVL_LOC_RAM = 0x80; /** Local DRAM */
enum PERF_MEM_LVL_REM_RAM1 = 0x100; /** Remote DRAM (1 hop) */
enum PERF_MEM_LVL_REM_RAM2 = 0x200; /** Remote DRAM (2 hops) */
enum PERF_MEM_LVL_REM_CCE1 = 0x400; /** Remote Cache (1 hop) */
enum PERF_MEM_LVL_REM_CCE2 = 0x800; /** Remote Cache (2 hops) */
enum PERF_MEM_LVL_IO = 0x1000; /** I/O memory */
enum PERF_MEM_LVL_UNC = 0x2000; /** Uncached memory */
///
enum PERF_MEM_LVL_SHIFT = 5;

enum PERF_MEM_REMOTE_REMOTE = 0x01; /** Remote */
///
enum PERF_MEM_REMOTE_SHIFT = 37;

enum PERF_MEM_LVLNUM_L1 = 0x01; /** L1 */
enum PERF_MEM_LVLNUM_L2 = 0x02; /** L2 */
enum PERF_MEM_LVLNUM_L3 = 0x03; /** L3 */
enum PERF_MEM_LVLNUM_L4 = 0x04; /** L4 */
/* 5-0xa available */
enum PERF_MEM_LVLNUM_ANY_CACHE = 0x0b; /** Any cache */
enum PERF_MEM_LVLNUM_LFB = 0x0c; /** LFB */
enum PERF_MEM_LVLNUM_RAM = 0x0d; /** RAM */
enum PERF_MEM_LVLNUM_PMEM = 0x0e; /** PMEM */
enum PERF_MEM_LVLNUM_NA = 0x0f; /** N/A */
///
enum PERF_MEM_LVLNUM_SHIFT = 33;

/* snoop mode */
enum PERF_MEM_SNOOP_NA = 0x01; /** not available */
enum PERF_MEM_SNOOP_NONE = 0x02; /** no snoop */
enum PERF_MEM_SNOOP_HIT = 0x04; /** snoop hit */
enum PERF_MEM_SNOOP_MISS = 0x08; /** snoop miss */
enum PERF_MEM_SNOOP_HITM = 0x10; /** snoop hit modified */
///
enum PERF_MEM_SNOOP_SHIFT = 19;

enum PERF_MEM_SNOOPX_FWD = 0x01; /** forward */
/** 1 free */
enum PERF_MEM_SNOOPX_SHIFT = 37;

/** locked instruction */
enum PERF_MEM_LOCK_NA = 0x01; /** not available */
enum PERF_MEM_LOCK_LOCKED = 0x02; /** locked transaction */
///
enum PERF_MEM_LOCK_SHIFT = 24;

/* TLB access */
enum PERF_MEM_TLB_NA = 0x01; /** not available */
enum PERF_MEM_TLB_HIT = 0x02; /** hit level */
enum PERF_MEM_TLB_MISS = 0x04; /** miss level */
enum PERF_MEM_TLB_L1 = 0x08; /** L1 */
enum PERF_MEM_TLB_L2 = 0x10; /** L2 */
enum PERF_MEM_TLB_WK = 0x20; /** Hardware Walker*/
enum PERF_MEM_TLB_OS = 0x40; /** OS fault handler */
///
enum PERF_MEM_TLB_SHIFT = 26;

/**
 * single taken branch record layout:
 *
 *      from: source instruction (may not always be a branch insn)
 *        to: branch target
 *   mispred: branch target was mispredicted
 * predicted: branch target was predicted
 *
 * support for mispred, predicted is optional. In case it
 * is not supported mispred = predicted = 0.
 *
 *     in_tx: running in a hardware transaction
 *     abort: aborting a hardware transaction
 *    cycles: cycles from last branch (or 0 if not supported)
 *      type: branch type
 */
struct perf_branch_entry
{
        ///
        ulong from;
        ///
        ulong to;

        /*  mixin(bitfields!(ulong, "mispred", 1, ulong, "predicted", 1, ulong,
            "in_tx", 1, ulong, "abort", 1, ulong, "cycles", 16, ulong, "type",
            4, ulong, "reserved", 40)); */
        private ulong perf_branch_entry_bitmanip;
        ///
        @property ulong mispred() @safe pure nothrow @nogc const
        {
                auto result = (perf_branch_entry_bitmanip & 1U) >> 0U;
                return cast(ulong) result;
        }
        ///
        @property void mispred(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= mispred_min,
                                "Value is smaller than the minimum value of bitfield 'mispred'");
                assert(v <= mispred_max,
                                "Value is greater than the maximum value of bitfield 'mispred'");
                perf_branch_entry_bitmanip = cast(typeof(perf_branch_entry_bitmanip))(
                                (perf_branch_entry_bitmanip & (-1 - cast(typeof(perf_branch_entry_bitmanip)) 1U)) | (
                                (cast(typeof(perf_branch_entry_bitmanip)) v << 0U) & 1U));
        }

        enum ulong mispred_min = cast(ulong) 0U;
        enum ulong mispred_max = cast(ulong) 1U;
        ///
        @property ulong predicted() @safe pure nothrow @nogc const
        {
                auto result = (perf_branch_entry_bitmanip & 2U) >> 1U;
                return cast(ulong) result;
        }
        ///
        @property void predicted(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= predicted_min,
                                "Value is smaller than the minimum value of bitfield 'predicted'");
                assert(v <= predicted_max,
                                "Value is greater than the maximum value of bitfield 'predicted'");
                perf_branch_entry_bitmanip = cast(typeof(perf_branch_entry_bitmanip))(
                                (perf_branch_entry_bitmanip & (-1 - cast(typeof(perf_branch_entry_bitmanip)) 2U)) | (
                                (cast(typeof(perf_branch_entry_bitmanip)) v << 1U) & 2U));
        }

        enum ulong predicted_min = cast(ulong) 0U;
        enum ulong predicted_max = cast(ulong) 1U;
        ///
        @property ulong in_tx() @safe pure nothrow @nogc const
        {
                auto result = (perf_branch_entry_bitmanip & 4U) >> 2U;
                return cast(ulong) result;
        }
        ///
        @property void in_tx(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= in_tx_min,
                                "Value is smaller than the minimum value of bitfield 'in_tx'");
                assert(v <= in_tx_max,
                                "Value is greater than the maximum value of bitfield 'in_tx'");
                perf_branch_entry_bitmanip = cast(typeof(perf_branch_entry_bitmanip))(
                                (perf_branch_entry_bitmanip & (-1 - cast(typeof(perf_branch_entry_bitmanip)) 4U)) | (
                                (cast(typeof(perf_branch_entry_bitmanip)) v << 2U) & 4U));
        }

        enum ulong in_tx_min = cast(ulong) 0U;
        enum ulong in_tx_max = cast(ulong) 1U;
        ///
        @property ulong abort() @safe pure nothrow @nogc const
        {
                auto result = (perf_branch_entry_bitmanip & 8U) >> 3U;
                return cast(ulong) result;
        }
        ///
        @property void abort(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= abort_min,
                                "Value is smaller than the minimum value of bitfield 'abort'");
                assert(v <= abort_max,
                                "Value is greater than the maximum value of bitfield 'abort'");
                perf_branch_entry_bitmanip = cast(typeof(perf_branch_entry_bitmanip))(
                                (perf_branch_entry_bitmanip & (-1 - cast(typeof(perf_branch_entry_bitmanip)) 8U)) | (
                                (cast(typeof(perf_branch_entry_bitmanip)) v << 3U) & 8U));
        }

        enum ulong abort_min = cast(ulong) 0U;
        enum ulong abort_max = cast(ulong) 1U;
        ///
        @property ulong cycles() @safe pure nothrow @nogc const
        {
                auto result = (perf_branch_entry_bitmanip & 1048560U) >> 4U;
                return cast(ulong) result;
        }
        ///
        @property void cycles(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= cycles_min,
                                "Value is smaller than the minimum value of bitfield 'cycles'");
                assert(v <= cycles_max,
                                "Value is greater than the maximum value of bitfield 'cycles'");
                perf_branch_entry_bitmanip = cast(typeof(perf_branch_entry_bitmanip))(
                                (perf_branch_entry_bitmanip & (-1 - cast(typeof(perf_branch_entry_bitmanip)) 1048560U)) | (
                                (cast(typeof(perf_branch_entry_bitmanip)) v << 4U) & 1048560U));
        }

        enum ulong cycles_min = cast(ulong) 0U;
        enum ulong cycles_max = cast(ulong) 65535U;
        ///
        @property ulong type() @safe pure nothrow @nogc const
        {
                auto result = (perf_branch_entry_bitmanip & 15728640U) >> 20U;
                return cast(ulong) result;
        }
        ///
        @property void type(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= type_min, "Value is smaller than the minimum value of bitfield 'type'");
                assert(v <= type_max, "Value is greater than the maximum value of bitfield 'type'");
                perf_branch_entry_bitmanip = cast(typeof(perf_branch_entry_bitmanip))(
                                (perf_branch_entry_bitmanip & (-1 - cast(typeof(perf_branch_entry_bitmanip)) 15728640U)) | (
                                (cast(typeof(perf_branch_entry_bitmanip)) v << 20U) & 15728640U));
        }

        enum ulong type_min = cast(ulong) 0U;
        enum ulong type_max = cast(ulong) 15U;
        ///
        @property ulong reserved() @safe pure nothrow @nogc const
        {
                auto result = (perf_branch_entry_bitmanip & 18446744073692774400UL) >> 24U;
                return cast(ulong) result;
        }
        ///
        @property void reserved(ulong v) @safe pure nothrow @nogc
        {
                assert(v >= reserved_min,
                                "Value is smaller than the minimum value of bitfield 'reserved'");
                assert(v <= reserved_max,
                                "Value is greater than the maximum value of bitfield 'reserved'");
                perf_branch_entry_bitmanip = cast(typeof(perf_branch_entry_bitmanip))(
                                (perf_branch_entry_bitmanip & (-1 - cast(
                                typeof(perf_branch_entry_bitmanip)) 18446744073692774400UL)) | (
                                (cast(typeof(perf_branch_entry_bitmanip)) v << 24U) & 18446744073692774400UL));
        }

        enum ulong reserved_min = cast(ulong) 0U;
        enum ulong reserved_max = cast(ulong) 1099511627775UL;
}
