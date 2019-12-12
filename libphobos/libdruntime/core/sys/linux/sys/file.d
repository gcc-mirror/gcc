/**
 * D header file for Linux file ops.
 *
 * Copyright: Copyright Nemanja Boric 2016.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Nemanja Boric
 */
module core.sys.linux.sys.file;

version (linux):
extern (C):
nothrow:
@nogc:

/* Operations for the `flock' call. */
/// Shared lock
enum LOCK_SH              = 0x01;
/// Exclusive lock
enum LOCK_EX              = 0x02;
/// Unlock
enum LOCK_UN              = 0x08;

/// Don't block when locking.
/// Can be OR'd into one of the above.
enum LOCK_NB              = 0x04;

/// Apply or remove an advisory lock on an open file
/// Params:
///     fd = file to apply or remove lock from
///     operation = lock operation to perform
/// Returns:
///     0 on success, -1 on failure, with .errno
///     set appropriately.
int     flock(int fd, int operation) @trusted;
