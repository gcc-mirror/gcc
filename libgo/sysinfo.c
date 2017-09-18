/* sysinfo.c -- input for mksysinfo.sh

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* This file is passed to GCC with the -fdump-go-spec option to
   generate a Go version of the system information.  */

#include "config.h"

#include <stddef.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <ucontext.h>
#include <netinet/in.h>
/* <netinet/tcp.h> needs u_char/u_short, but <sys/bsd_types> is only
   included by <netinet/in.h> if _SGIAPI (i.e. _SGI_SOURCE
   && !_XOPEN_SOURCE.
   <sys/termios.h> only defines TIOCNOTTY if !_XOPEN_SOURCE, while
   <sys/ttold.h> does so unconditionally.  */
#ifdef __sgi__
#include <sys/bsd_types.h>
#include <sys/ttold.h>
#endif
#include <netinet/tcp.h>
#if defined(HAVE_NETINET_IN_SYSTM_H)
#include <netinet/in_systm.h>
#endif
#if defined(HAVE_NETINET_IP_H)
#include <netinet/ip.h>
#endif
#if defined(HAVE_NETINET_IP_MROUTE_H)
#include <netinet/ip_mroute.h>
#endif
#if defined(HAVE_NETINET_IF_ETHER_H)
#include <netinet/if_ether.h>
#endif
#include <signal.h>
#include <sys/ioctl.h>
#include <termios.h>
#if defined(HAVE_SYSCALL_H)
#include <syscall.h>
#endif
#if defined(HAVE_SYS_SYSCALL_H)
#include <sys/syscall.h>
#endif
#if defined(HAVE_SYS_EPOLL_H)
#include <sys/epoll.h>
#endif
#if defined(HAVE_SYS_EVENT_H)
#include <sys/event.h>
#endif
#if defined(HAVE_SYS_FILE_H)
#include <sys/file.h>
#endif
#if defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif
#if defined(HAVE_SYS_PRCTL_H)
#include <sys/prctl.h>
#endif
#if defined(HAVE_SYS_PTRACE_H)
#include <sys/ptrace.h>
#endif
#include <sys/resource.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/wait.h>
#include <sys/un.h>
#if defined(HAVE_SYS_USER_H)
#include <sys/user.h>
#endif
#if defined(HAVE_SYS_UTSNAME_H)
#include <sys/utsname.h>
#endif
#if defined(HAVE_SYS_SELECT_H)
#include <sys/select.h>
#endif
#include <time.h>
#include <unistd.h>
#include <netdb.h>
#include <pwd.h>
#include <grp.h>
#if defined(HAVE_LINUX_FILTER_H)
#include <linux/filter.h>
#endif
#if defined(HAVE_LINUX_IF_ADDR_H)
#include <linux/if_addr.h>
#endif
#if defined(HAVE_LINUX_IF_ETHER_H)
#include <linux/if_ether.h>
#endif
#if defined(HAVE_LINUX_IF_TUN_H)
#include <linux/if_tun.h>
#endif
#if defined(HAVE_LINUX_NETLINK_H)
#include <linux/netlink.h>
#endif
#if defined(HAVE_LINUX_PTRACE_H)
/* Avoid https://sourceware.org/bugzilla/show_bug.cgi?id=762 .  */
#define ia64_fpreg pt_ia64_fpreg
#define pt_all_user_regs pt_ia64_all_user_regs
/* Avoid redefinition of ptrace_peeksiginfo from <sys/ptrace.h>.
   https://gcc.gnu.org/PR81324 .  */
#define ptrace_peeksiginfo_args ignore_ptrace_peeksiginfo_args
#include <linux/ptrace.h>
#undef ia64_fpreg
#undef pt_all_user_regs
#undef ptrace_peeksiginfo_args
#endif
#if defined(HAVE_LINUX_RTNETLINK_H)
#include <linux/rtnetlink.h>
#endif
#if defined(HAVE_NET_IF_H)
#include <net/if.h>
#endif
#if defined(HAVE_NET_IF_ARP_H)
#include <net/if_arp.h>
#endif
#if defined(HAVE_NET_ROUTE_H)
#include <net/route.h>
#endif
#if defined (HAVE_NETPACKET_PACKET_H)
#include <netpacket/packet.h>
#endif
#if defined(HAVE_SYS_MOUNT_H)
#include <sys/mount.h>
#endif
#if defined(HAVE_SYS_VFS_H)
#include <sys/vfs.h>
#endif
#if defined(HAVE_STATFS_H)
#include <sys/statfs.h>
#endif
#if defined(HAVE_SYS_TIMEX_H)
#include <sys/timex.h>
#endif
#if defined(HAVE_SYS_SYSINFO_H)
#include <sys/sysinfo.h>
#endif
#if defined(HAVE_USTAT_H)
#include <ustat.h>
#endif
#if defined(HAVE_UTIME_H)
#include <utime.h>
#endif
#if defined(HAVE_LINUX_ETHER_H)
#include <linux/ether.h>
#endif
#if defined(HAVE_LINUX_FS_H)
#include <linux/fs.h>
#endif
#if defined(HAVE_LINUX_REBOOT_H)
#include <linux/reboot.h>
#endif
#if defined(HAVE_SYS_INOTIFY_H)
#include <sys/inotify.h>
#endif
#if defined(HAVE_NETINET_ICMP6_H)
#include <netinet/icmp6.h>
#endif
#if defined(HAVE_SCHED_H)
#include <sched.h>
#endif
#if defined(HAVE_SEMAPHORE_H)
#include <semaphore.h>
#endif
#if defined(HAVE_PORT_H)
#include <port.h>
#endif

#ifdef USE_LIBFFI
#include "ffi.h"
#endif

/* Constants that may only be defined as expressions on some systems,
   expressions too complex for -fdump-go-spec to handle.  These are
   handled specially below.  */
enum {
#ifdef TIOCGWINSZ
  TIOCGWINSZ_val = TIOCGWINSZ,
#endif
#ifdef TIOCSWINSZ
  TIOCSWINSZ_val = TIOCSWINSZ,
#endif
#ifdef TIOCNOTTY
  TIOCNOTTY_val = TIOCNOTTY,
#endif
#ifdef TIOCSCTTY
  TIOCSCTTY_val = TIOCSCTTY,
#endif
#ifdef TIOCGPGRP
  TIOCGPGRP_val = TIOCGPGRP,
#endif
#ifdef TIOCSPGRP
  TIOCSPGRP_val = TIOCSPGRP,
#endif
#ifdef TIOCGPTN
  TIOCGPTN_val = TIOCGPTN,
#endif
#ifdef TIOCSPTLCK
  TIOCSPTLCK_val = TIOCSPTLCK,
#endif
#ifdef TIOCGDEV
  TIOCGDEV_val = TIOCGDEV,
#endif
#ifdef TIOCSIG
  TIOCSIG_val = TIOCSIG,
#endif
#ifdef TCGETS
  TCGETS_val = TCGETS,
#endif
#ifdef TCSETS
  TCSETS_val = TCSETS,
#endif
#ifdef TUNSETIFF
  TUNSETIFF_val = TUNSETIFF,
#endif
#ifdef TUNSETNOCSUM
  TUNSETNOCSUM_val = TUNSETNOCSUM,
#endif
#ifdef TUNSETDEBUG
  TUNSETDEBUG_val = TUNSETDEBUG,
#endif
#ifdef TUNSETPERSIST
  TUNSETPERSIST_val = TUNSETPERSIST,
#endif
#ifdef TUNSETOWNER
  TUNSETOWNER_val = TUNSETOWNER,
#endif
#ifdef TUNSETLINK
  TUNSETLINK_val = TUNSETLINK,
#endif
#ifdef TUNSETGROUP
  TUNSETGROUP_val = TUNSETGROUP,
#endif
#ifdef TUNGETFEATURES
  TUNGETFEATURES_val = TUNGETFEATURES,
#endif
#ifdef TUNSETOFFLOAD
  TUNSETOFFLOAD_val = TUNSETOFFLOAD,
#endif
#ifdef TUNSETTXFILTER
  TUNSETTXFILTER_val = TUNSETTXFILTER,
#endif
#ifdef TUNGETIFF
  TUNGETIFF_val = TUNGETIFF,
#endif
#ifdef TUNGETSNDBUF
  TUNGETSNDBUF_val = TUNGETSNDBUF,
#endif
#ifdef TUNSETSNDBUF
  TUNSETSNDBUF_val = TUNSETSNDBUF,
#endif
#ifdef TUNATTACHFILTER
  TUNATTACHFILTER_val = TUNATTACHFILTER,
#endif
#ifdef TUNDETACHFILTER
  TUNDETACHFILTER_val = TUNDETACHFILTER,
#endif
#ifdef TUNGETVNETHDRSZ
  TUNGETVNETHDRSZ_val = TUNGETVNETHDRSZ,
#endif
#ifdef TUNSETVNETHDRSZ
  TUNSETVNETHDRSZ_val = TUNSETVNETHDRSZ,
#endif
#ifdef TUNSETQUEUE
  TUNSETQUEUE_val = TUNSETQUEUE,
#endif
#ifdef TUNSETIFINDEX
  TUNSETIFINDEX_val = TUNSETIFINDEX,
#endif
#ifdef TUNGETFILTER
  TUNGETFILTER_val = TUNGETFILTER,
#endif
#ifdef NLA_HDRLEN
  NLA_HDRLEN_val = NLA_HDRLEN,
#endif
};

#if defined(HAVE_SYS_EPOLL_H)
enum {
  epoll_data_offset = offsetof(struct epoll_event, data)
};
#endif
