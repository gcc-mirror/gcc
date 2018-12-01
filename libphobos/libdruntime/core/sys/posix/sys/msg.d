/**
* D header file for POSIX.
*
* Authors: Neven Miculinić
*/

module core.sys.posix.sys.msg;

import core.sys.posix.sys.ipc;
public import core.sys.posix.sys.types;
public import core.stdc.config;

version (CRuntime_Glibc):
// Some of these may be from linux kernel headers.
extern (C):

public enum MSG_STAT = 11;
public enum MSG_INFO = 12;

public enum MSG_NOERROR = 1 << 12; // octal!10000
public enum  MSG_EXCEPT = 2 << 12; // octal!20000
public enum    MSG_COPY = 4 << 12; // octal!40000

struct msgbuf
{
    c_long mtype;
    char[1] mtext;
}

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

version (Alpha)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/alpha/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        time_t msg_stime;
        time_t msg_rtime;
        time_t msg_ctime;
        c_ulong __msg_cbytes;
        msgqnum_t msg_qnum;
        msglen_t msg_qbytes;
        pid_t msg_lspid;
        pid_t msg_lrpid;
        c_ulong __glibc_reserved1;
        c_ulong __glibc_reserved2;
    }
}
else version (HPPA)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/hppa/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    // Assuming word size is 32
    struct msqid_ds
    {
        ipc_perm msg_perm;
        c_ulong __pad1;
        time_t          msg_stime;
        c_ulong __pad2;
        time_t          msg_rtime;
        c_ulong __pad3;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved1;
        c_ulong __glibc_reserved2;
    }

}
else version (MIPS32)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/mips/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm  msg_perm;
        version (BigEndian) c_ulong __glibc_reserved1;
        time_t    msg_stime;
        version (LittleEndian) c_ulong __glibc_reserved1;
        version (BigEndian) c_ulong __glibc_reserved2;
        time_t    msg_rtime;
        version (LittleEndian) c_ulong __glibc_reserved2;
        version (BigEndian) c_ulong __glibc_reserved3;
        time_t    msg_ctime;
        version (LittleEndian) c_ulong __glibc_reserved3;
        c_ulong   __msg_cbytes;
        msgqnum_t msg_qnum;
        msglen_t  msg_qbytes;
        pid_t     msg_lspid;
        pid_t     msg_lrpid;
        c_ulong   __glibc_reserved4;
        c_ulong   __glibc_reserved5;
    }
}
else version (MIPS64)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/mips/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm  msg_perm;
        time_t    msg_stime;
        time_t    msg_rtime;
        time_t    msg_ctime;
        c_ulong   __msg_cbytes;
        msgqnum_t msg_qnum;
        msglen_t  msg_qbytes;
        pid_t     msg_lspid;
        pid_t     msg_lrpid;
        c_ulong   __glibc_reserved4;
        c_ulong   __glibc_reserved5;
    }
}
else version (PPC)
{

    //  https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/powerpc/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        c_ulong __glibc_reserved1;
        time_t          msg_stime;
        c_ulong __glibc_reserved2;
        time_t          msg_rtime;
        c_ulong __glibc_reserved3;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
}
else version (PPC64)
{
    //  https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/powerpc/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm  msg_perm;
        time_t    msg_stime;
        time_t    msg_rtime;
        time_t    msg_ctime;
        c_ulong   __msg_cbytes;
        msgqnum_t msg_qnum;
        msglen_t  msg_qbytes;
        pid_t     msg_lspid;
        pid_t     msg_lrpid;
        c_ulong   __glibc_reserved4;
        c_ulong   __glibc_reserved5;
    }
}
else version (RISCV32)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/generic/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        c_ulong __glibc_reserved1;
        time_t          msg_stime;
        c_ulong __glibc_reserved2;
        time_t          msg_rtime;
        c_ulong __glibc_reserved3;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
}
else version (RISCV64)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/generic/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        time_t          msg_stime;
        time_t          msg_rtime;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
}
else version (S390)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/s390/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    // Assuming wordsize != 64
    struct msqid_ds
    {
        ipc_perm msg_perm;
        c_ulong __glibc_reserved1;
        time_t          msg_stime;
        c_ulong __glibc_reserved2;
        time_t          msg_rtime;
        c_ulong __glibc_reserved3;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
}
else version (SystemZ)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/s390/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    // Assuming wordsize == 64
    struct msqid_ds
    {
        ipc_perm msg_perm;
        time_t          msg_stime;
        time_t          msg_rtime;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
}
else version (SPARC)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/sparc/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    // Assuming word size is 32
    struct msqid_ds
    {
        ipc_perm msg_perm;
        c_ulong __pad1;
        time_t          msg_stime;
        c_ulong __pad2;
        time_t          msg_rtime;
        c_ulong __pad3;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved1;
        c_ulong __glibc_reserved2;
    }
}
else version (SPARC64)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/sparc/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    // Assuming word size is 32
    struct msqid_ds
    {
        ipc_perm msg_perm;
        c_ulong __pad1;
        time_t          msg_stime;
        c_ulong __pad2;
        time_t          msg_rtime;
        c_ulong __pad3;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved1;
        c_ulong __glibc_reserved2;
    }
}
else version (X86)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/x86/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        time_t          msg_stime;
        time_t          msg_rtime;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
}
else version (X86_64)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/x86/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        c_ulong __glibc_reserved1;
        time_t          msg_stime;
        c_ulong __glibc_reserved2;
        time_t          msg_rtime;
        c_ulong __glibc_reserved3;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
}
else version (AArch64)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/generic/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        time_t          msg_stime;
        time_t          msg_rtime;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
}
else version (ARM)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/generic/bits/msq.h
    alias c_ulong msgqnum_t;
    alias c_ulong msglen_t;

    struct msqid_ds
    {
        ipc_perm msg_perm;
        c_ulong __glibc_reserved1;
        time_t          msg_stime;
        c_ulong __glibc_reserved2;
        time_t          msg_rtime;
        c_ulong __glibc_reserved3;
        time_t          msg_ctime;
        c_ulong         __msg_cbytes;
        msgqnum_t       msg_qnum;
        msglen_t        msg_qbytes;
        pid_t           msg_lspid;
        pid_t           msg_lrpid;
        c_ulong __glibc_reserved4;
        c_ulong __glibc_reserved5;
    }
} else
    static assert(0, "unimplemented");


public enum MSG_MEM_SCALE =  32;
public enum MSGMNI =     16;
public enum MSGMAX =   8192;
public enum MSGMNB =  16384;

int msgctl (int msqid, int cmd, msqid_ds *__buf);
int msgget ( key_t key, int msgflg );
ssize_t msgrcv(int msqid, void *msgp, size_t msgsz, c_long msgtyp, int msgflg);
int msgsnd ( int msqid, msgbuf *msgp, int msgsz, int msgflg );
