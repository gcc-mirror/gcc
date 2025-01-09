/**
 * D header file for GNU/Linux.
 *
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Teodor Dutu
 */

module core.sys.linux.sys.procfs;

version (linux):

import core.sys.posix.sys.types : pid_t;

alias lwpid_t = pid_t;
