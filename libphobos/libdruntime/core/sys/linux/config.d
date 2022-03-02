/**
 * D header file for GNU/Linux
 *
 * Authors: Martin Nowak
 */
module core.sys.linux.config;

version (linux):

public import core.sys.posix.config;

// man 7 feature_test_macros
// http://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html
enum _GNU_SOURCE = true;
// deduced <features.h>
// http://sourceware.org/git/?p=glibc.git;a=blob;f=include/features.h
enum _DEFAULT_SOURCE = true;
enum _ATFILE_SOURCE = true;

// _BSD_SOURCE and _SVID_SOURCE are deprecated aliases for _DEFAULT_SOURCE.
deprecated("use _DEFAULT_SOURCE")
{
    enum _BSD_SOURCE = true;
    enum _SVID_SOURCE = true;
}

deprecated("use _DEFAULT_SOURCE")
enum __USE_MISC = _DEFAULT_SOURCE;
deprecated("use _ATFILE_SOURCE")
enum __USE_ATFILE = _ATFILE_SOURCE;
deprecated("use _GNU_SOURCE")
enum __USE_GNU = _GNU_SOURCE;
