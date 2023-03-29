/**
 * D header file for GNU/Linux.
 *
 * Copyright: Copyright Robert Klotzner 2012.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Robert Klotzner
 */
module core.sys.linux.sys.xattr;

import core.sys.posix.sys.types;

version (linux):
extern (C):
nothrow:
@nogc:

enum {
    XATTR_CREATE = 1, /* set value, fail if attr already exists.  */
    XATTR_REPLACE = 2 /* set value, fail if attr does not exist.  */
}

enum XATTR_OS2_PREFIX = "os2.";
enum XATTR_OS2_PREFIX_LEN = XATTR_OS2_PREFIX.length;
enum XATTR_SECURITY_PREFIX = "security.";
enum XATTR_SECURITY_PREFIX_LEN = XATTR_SECURITY_PREFIX.length;
enum XATTR_SYSTEM_PREFIX = "system.";
enum XATTR_SYSTEM_PREFIX_LEN = XATTR_SYSTEM_PREFIX.length;
enum XATTR_TRUSTED_PREFIX = "trusted.";
enum XATTR_TRUSTED_PREFIX_LEN = XATTR_TRUSTED_PREFIX.length;
enum XATTR_USER_PREFIX = "user.";
enum XATTR_USER_PREFIX_LEN = XATTR_USER_PREFIX.length;

/* Security namespace */
enum XATTR_SELINUX_SUFFIX = "selinux.";
enum XATTR_NAME_SELINUX = XATTR_SECURITY_PREFIX ~ XATTR_SELINUX_SUFFIX;

enum XATTR_SMACK_SUFFIX = "SMACK64";
enum XATTR_SMACK_IPIN = "SMACK64IPIN";
enum XATTR_SMACK_IPOUT = "SMACK64IPOUT";
enum XATTR_SMACK_EXEC = "SMACK64EXEC";
enum XATTR_SMACK_TRANSMUTE = "SMACK64TRANSMUTE";
enum XATTR_SMACK_MMAP = "SMACK64MMAP";

enum XATTR_NAME_SMACK = XATTR_SECURITY_PREFIX ~ XATTR_SMACK_SUFFIX;
enum XATTR_NAME_SMACKIPIN = XATTR_SECURITY_PREFIX ~ XATTR_SMACK_IPIN;
enum XATTR_NAME_SMACKIPOUT = XATTR_SECURITY_PREFIX ~ XATTR_SMACK_IPOUT;
enum XATTR_NAME_SMACKEXEC = XATTR_SECURITY_PREFIX ~ XATTR_SMACK_EXEC;
enum XATTR_NAME_SMACKTRANSMUTE = XATTR_SECURITY_PREFIX ~ XATTR_SMACK_TRANSMUTE;
enum XATTR_NAME_SMACKMMAP = XATTR_SECURITY_PREFIX ~ XATTR_SMACK_MMAP;

enum XATTR_CAPS_SUFFIX = "capability";
enum XATTR_NAME_CAPS = XATTR_SECURITY_PREFIX ~ XATTR_CAPS_SUFFIX;


int setxattr(const scope char* path, const scope char* name, const scope void* value, size_t size, int flags);

int lsetxattr(const scope char* path, const scope char* name, const scope void* value, size_t size, int flags);
int fsetxattr(int fd, const scope char* name, const scope void* value, size_t size, int flags);
ssize_t getxattr(const scope char* path, const scope char* name, void* value, size_t size);
ssize_t lgetxattr(const scope char* path, const scope char* name, void* value, size_t size);
ssize_t fgetxattr(int fd, const scope char* name, void* value, size_t size);
ssize_t listxattr(const scope char* path, char* list, size_t size);
ssize_t llistxattr(const scope char* path, char* list, size_t size);
ssize_t flistxattr (int __fd, char *list, size_t size);
int removexattr (const scope char *path, const scope char *name);
int lremovexattr (const scope char *path, const scope char *name);
int fremovexattr (int fd, const scope char *name);
