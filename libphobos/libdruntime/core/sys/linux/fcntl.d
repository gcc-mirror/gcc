module core.sys.linux.fcntl;

public import core.sys.posix.fcntl;

version (linux):
extern(C):
nothrow:
@system:

// From linux/falloc.h
/// fallocate(2) params
enum {
    /// Allocates and initializes to zero the disk space
    /// within the specified range, but the file size
    /// will not be modified.
    FALLOC_FL_KEEP_SIZE = 0x01,
    /// Deallocates space (i.e. creates a hole)
    FALLOC_FL_PUNCH_HOLE = 0x02,
    /// Newly allocated blocks will be marked as initialized.
    FALLOC_FL_NO_HIDE_STALE = 0x04,
    /// Removes a byte range from a file, without leaving a hole
    FALLOC_FL_COLLAPSE_RANGE = 0x08,
    /// Zeroes space in the specified byte range
    FALLOC_FL_ZERO_RANGE = 0x10,
    /// Increases the file space by inserting a hole
    /// without overwriting any existing data
    FALLOC_FL_INSERT_RANGE = 0x20,
    /// Used to unshare shared blocks within
    /// the file size without overwriting any existing data
    FALLOC_FL_UNSHARE_RANGE = 0x40
}

// From asm-generic/fcntl.h
/**

Open File Description locks

Usually record locks held by a process are released on *any* close and are
not inherited across a fork().

These cmd values will set locks that conflict with process-associated
record  locks, but are "owned" by the open file description, not the
process. This means that they are inherited across fork() like BSD (flock)
locks, and they are only released automatically when the last reference to
the the open file against which they were acquired is put.

*/
enum
{
    /// Queries the system if the lock could be placed
    F_OFD_GETLK  = 36,
    /// Acquires or releases an open file description lock
    F_OFD_SETLK  = 37,
    /// Same as F_OFD_SETLK, but waits if a conflicting lock is held on the file
    F_OFD_SETLKW = 38
}

// Linux-specific fallocate
// (http://man7.org/linux/man-pages/man2/fallocate.2.html)
int fallocate(int fd, int mode, off_t offset, off_t len);
