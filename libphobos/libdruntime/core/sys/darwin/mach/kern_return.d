/**
 * D header file for Darwin.
 *
 * Copyright: Copyright Sean Kelly 2008 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 */

/*          Copyright Sean Kelly 2008 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.darwin.mach.kern_return;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):

alias int kern_return_t;

enum : kern_return_t
{
    KERN_SUCCESS                = 0,
    KERN_INVALID_ADDRESS        = 1,
    KERN_PROTECTION_FAILURE     = 2,
    KERN_NO_SPACE               = 3,
    KERN_INVALID_ARGUMENT       = 4,
    KERN_FAILURE                = 5,
    KERN_RESOURCE_SHORTAGE      = 6,
    KERN_NOT_RECEIVER           = 7,
    KERN_NO_ACCESS              = 8,
    KERN_MEMORY_FAILURE         = 9,
    KERN_MEMORY_ERROR           = 10,
    KERN_ALREADY_IN_SET         = 11,
    KERN_NOT_IN_SET             = 12,
    KERN_NAME_EXISTS            = 13,
    KERN_ABORTED                = 14,
    KERN_INVALID_NAME           = 15,
    KERN_INVALID_TASK           = 16,
    KERN_INVALID_RIGHT          = 17,
    KERN_INVALID_VALUE          = 18,
    KERN_UREFS_OVERFLOW         = 19,
    KERN_INVALID_CAPABILITY     = 20,
    KERN_RIGHT_EXISTS           = 21,
    KERN_INVALID_HOST           = 22,
    KERN_MEMORY_PRESENT         = 23,
    KERN_MEMORY_DATA_MOVED      = 24,
    KERN_MEMORY_RESTART_COPY    = 25,
    KERN_INVALID_PROCESSOR_SET  = 26,
    KERN_POLICY_LIMIT           = 27,
    KERN_INVALID_POLICY         = 28,
    KERN_INVALID_OBJECT         = 29,
    KERN_ALREADY_WAITING        = 30,
    KERN_DEFAULT_SET            = 31,
    KERN_EXCEPTION_PROTECTED    = 32,
    KERN_INVALID_LEDGER         = 33,
    KERN_INVALID_MEMORY_CONTROL = 34,
    KERN_INVALID_SECURITY       = 35,
    KERN_NOT_DEPRESSED          = 36,
    KERN_TERMINATED             = 37,
    KERN_LOCK_SET_DESTROYED     = 38,
    KERN_LOCK_UNSTABLE          = 39,
    KERN_LOCK_OWNED             = 40,
    KERN_LOCK_OWNED_SELF        = 41,
    KERN_SEMAPHORE_DESTROYED    = 42,
    KERN_RPC_SERVER_TERMINATED  = 43,
    KERN_RPC_TERMINATE_ORPHAN   = 44,
    KERN_RPC_CONTINUE_ORPHAN    = 45,
    KERN_NOT_SUPPORTED          = 46,
    KERN_NODE_DOWN              = 47,
    KERN_OPERATION_TIMED_OUT    = 49,
    KERN_RETURN_MAX             = 0x100,
}
