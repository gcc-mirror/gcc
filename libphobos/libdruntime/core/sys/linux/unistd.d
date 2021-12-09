module core.sys.linux.unistd;

version (linux):
extern (C):
nothrow:
@system:
@nogc:

public import core.sys.posix.unistd;
public import core.sys.linux.syscalls : SystemCall;
import core.stdc.config : c_long;

// Additional seek constants for sparse file handling
// from Linux's unistd.h, stdio.h, and linux/fs.h
// (see http://man7.org/linux/man-pages/man2/lseek.2.html)
enum
{
    /// Offset is relative to the next location containing data
    SEEK_DATA = 3,
    /// Offset is relative to the next hole (or EOF if file is not sparse)
    SEEK_HOLE = 4
}

/// Prompt for a password without echoing it.
char* getpass(const(char)* prompt);

// Exit all threads in a process
void exit_group(int status);

/**
Invoke system call specified by number, passing it the remaining arguments.
This is completely system-dependent, and not often useful.

In Unix, `syscall' sets `errno' for all errors and most calls return -1
for errors; in many systems you cannot pass arguments or get return
values for all system calls (`pipe', `fork', and `getppid' typically
among them).

In Mach, all system calls take normal arguments and always return an
error code (zero for success).
*/
c_long syscall(SystemCall number, ...) @nogc nothrow;
