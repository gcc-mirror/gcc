/**
* D header file for POSIX.
*
* Authors: Neven Miculinić
*/

module core.sys.posix.sys.msg;

import core.sys.posix.config;
import core.sys.posix.sys.ipc;
import core.sys.posix.sys.types;
import core.stdc.config;

version (CRuntime_Glibc):
// Some of these may be from linux kernel headers.
extern (C):

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (HPPA)    version = HPPA_Any;
version (HPPA64)  version = HPPA_Any;
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

version (linux)
{
    enum MSG_STAT = 11;
    enum MSG_INFO = 12;

    enum MSG_NOERROR = 1 << 12; // octal!10000
    enum  MSG_EXCEPT = 2 << 12; // octal!20000
    enum    MSG_COPY = 4 << 12; // octal!40000

    struct msginfo
    {
        int msgpool;
        int msgmap;
        int msgmax;
        int msgmnb;
        int msgmni;
        int msgssz;
        int msgtql;
        ushort msgseg;
    }

    version (Alpha)     version = GENERICMSQ;
    version (ARM_Any)   version = GENERICMSQ;
    version (IA64)      version = GENERICMSQ;
    version (IBMZ_Any)  version = GENERICMSQ;
    version (RISCV_Any) version = GENERICMSQ;

    version (GENERICMSQ)
    {
        // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/bits/msq-pad.h
        private enum MSQ_PAD_AFTER_TIME = (__WORDSIZE == 32);
        private enum MSQ_PAD_BEFORE_TIME = false;
    }
    else version (HPPA_Any)
    {
        // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/hppa/bits/msq-pad.h
        private enum MSQ_PAD_AFTER_TIME = false;
        private enum MSQ_PAD_BEFORE_TIME = (__WORDSIZE == 32);
    }
    else version (MIPS_Any)
    {
        // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/mips/bits/msq-pad.h
        version (LittleEndian)
        {
            private enum MSQ_PAD_AFTER_TIME = (__WORDSIZE == 32);
            private enum MSQ_PAD_BEFORE_TIME = false;
        }
        else
        {
            private enum MSQ_PAD_AFTER_TIME = false;
            private enum MSQ_PAD_BEFORE_TIME = (__WORDSIZE == 32);
        }
    }
    else version (PPC_Any)
    {
        //  https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/powerpc/bits/msq-pad.h
        private enum MSQ_PAD_AFTER_TIME = false;
        private enum MSQ_PAD_BEFORE_TIME = (__WORDSIZE == 32);
    }
    else version (SPARC_Any)
    {
        // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/sparc/bits/msq-pad.h
        private enum MSQ_PAD_AFTER_TIME = false;
        private enum MSQ_PAD_BEFORE_TIME = (__WORDSIZE == 32);
    }
    else version (X86)
    {
        // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/x86/bits/msq-pad.h
        private enum MSQ_PAD_AFTER_TIME = true;
        private enum MSQ_PAD_BEFORE_TIME = false;
    }
    else version (X86_64)
    {
        // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/x86/bits/msq-pad.h
        private enum MSQ_PAD_AFTER_TIME = false;
        private enum MSQ_PAD_BEFORE_TIME = false;
    }
    else
        static assert(0, "unimplemented");

    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/bits/msq.h
    alias msgqnum_t = ulong_t;
    alias msglen_t = ulong_t;

    static if (MSQ_PAD_BEFORE_TIME)
    {
        struct msqid_ds
        {
            ipc_perm        msg_perm;
            c_ulong __glibc_reserved1;
            time_t          msg_stime;
            c_ulong __glibc_reserved2;
            time_t          msg_rtime;
            c_ulong __glibc_reserved3;
            time_t          msg_ctime;
            ulong_t         __msg_cbytes;
            msgqnum_t       msg_qnum;
            msglen_t        msg_qbytes;
            pid_t           msg_lspid;
            pid_t           msg_lrpid;
            ulong_t __glibc_reserved4;
            ulong_t __glibc_reserved5;
        }
    }
    else static if (MSQ_PAD_AFTER_TIME)
    {
        struct msqid_ds
        {
            ipc_perm        msg_perm;
            time_t          msg_stime;
            c_ulong __glibc_reserved1;
            time_t          msg_rtime;
            c_ulong __glibc_reserved2;
            time_t          msg_ctime;
            c_ulong __glibc_reserved3;
            ulong_t         __msg_cbytes;
            msgqnum_t       msg_qnum;
            msglen_t        msg_qbytes;
            pid_t           msg_lspid;
            pid_t           msg_lrpid;
            ulong_t __glibc_reserved4;
            ulong_t __glibc_reserved5;
        }
    }
    else
    {
        struct msqid_ds
        {
            ipc_perm        msg_perm;
            time_t          msg_stime;
            time_t          msg_rtime;
            time_t          msg_ctime;
            ulong_t         __msg_cbytes;
            msgqnum_t       msg_qnum;
            msglen_t        msg_qbytes;
            pid_t           msg_lspid;
            pid_t           msg_lrpid;
            ulong_t __glibc_reserved4;
            ulong_t __glibc_reserved5;
        }
    }
}
else
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=bits/msq.h
    enum MSG_NOERROR = 1 << 12; // octal!10000

    alias msgqnum_t = ushort;
    alias msglen_t = ushort;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        time_t          msg_stime;
        time_t          msg_rtime;
        time_t          msg_ctime;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
    }
}

struct msgbuf
{
    c_long mtype;
    char[1] mtext = 0;
}

int msgctl(int msqid, int cmd, msqid_ds* __buf);
int msgget(key_t key, int msgflg);
ssize_t msgrcv(int msqid, void* msgp, size_t msgsz, c_long msgtyp, int msgflg);
int msgsnd(int msqid, msgbuf* msgp, int msgsz, int msgflg);
