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

// The following section introduces explicit references to types and
// constants of interest to support bootstrapping libgo using a
// compiler that doesn't support -fdump-go-spec (e.g., clang), via
// DWARF-based tools. This process is made more difficult due to the
// fact that clang tries hard to omit types/constants from DWARF if it
// can't find explicit references to them, so here we make sure that
// key items are mentioned in ways that will force them into the
// generated DWARF.

#if defined(__clang__)

// Make a reference to a type
#define TREF(typ) typ typ ## ref

// Make a reference to an opaque type
#define OTREF(typ) typ *typ ## ref

// Make a reference to a struct tag
#define SREF(stag) struct stag stag ## ref

// Make a reference to an enum literal
#define EREF(elit) unsigned elit ## fn(unsigned x) { return x == elit ? 1 : 0; }

//......................................................................

// From dirent.h
SREF(dirent);
SREF(dirent64);
OTREF(DIR);

// From fcntl.h
SREF(flock);
SREF(flock64);

// From ffi headers
SREF(_ffi_type);
TREF(ffi_cif);
TREF(ffi_abi);
TREF(ffi_status);
EREF(FFI_OK);

// From grp.h
SREF(group);

#if defined(HAVE_LINUX_FILTER_H)
// From linux/filter.h
SREF(sock_filter);
SREF(sock_fprog);
#endif

// From linux/if.h
EREF(IFF_UP);

#if defined(HAVE_LINUX_IF_ADDR_H)
// From linux/if_addr.h
SREF(ifaddrmsg);
EREF(IFA_ADDRESS);
#endif

#if defined(HAVE_LINUX_RTNETLINK_H)
// From linux/if_link.h
EREF(IFLA_ADDRESS);
#endif

// From in.h, in6.h, icmp6.h
SREF(ip_mreq);
SREF(ip_mreqn);
SREF(ipv6_mreq);
SREF(ip6_mtuinfo);
SREF(icmp6_filter);
SREF(in_pktinfo);
EREF(IPPROTO_TCP);

#if defined(HAVE_LINUX_RTNETLINK_H)
// From linux/rtnetlink.h
SREF(rtgenmsg);
SREF(rtmsg);
SREF(ifinfomsg);
SREF(rtattr);
SREF(rtnexthop);
EREF(RTM_BASE);
EREF(RTN_UNSPEC);
#endif

// From netdb.h
SREF(addrinfo);

// From netlink.h
SREF(nlattr);
SREF(nlmsgerr);

// From pthread.h and related
TREF(pthread_attr_t);
TREF(pthread_t);
TREF(pthread_mutex_t);
TREF(pthread_mutexattr_t);

// From pwd.h
SREF(passwd);

// From signal.h and related
TREF(sigset_t);
TREF(siginfo_t);
TREF(stack_t);
SREF(sigaction);
SREF(sigstack);
EREF(SI_USER);
EREF(FPE_INTOVF);
EREF(BUS_ADRALN);
EREF(SS_ONSTACK);
EREF(SEGV_MAPERR);

// From stat.h
SREF(stat64);

// From statfs.h
SREF(statfs);
SREF(statfs64);

// From sysinfo.h
SREF(sysinfo);

// From <sys/epoll.h>
#if defined(HAVE_SYS_EPOLL_H)
SREF(epoll_event);
EREF(EPOLLIN);
EREF(epoll_data_offset);
#endif

#if defined(HAVE_SYS_MOUNT_H)
// From sys/mount.h
EREF(MS_PRIVATE);
EREF(MNT_FORCE);
#endif

#if defined(HAVE_SYS_PTRACE_H)
// From <sys/ptrace.h>
SREF(pt_regs);
EREF(PTRACE_PEEKTEXT);
#endif

// From sys/resource.h
SREF(rusage);
SREF(rlimit64);
EREF(RLIMIT_NOFILE);
EREF(PRIO_USER);

// From sys/select.h
TREF(fd_set);

// From sys/socket.h
SREF(msghdr);
SREF(cmsghdr);
SREF(ucred);
EREF(MSG_OOB);
EREF(SCM_RIGHTS);
EREF(SOCK_RAW);
EREF(SHUT_RD);

// From sys/time.h and sys/times.h
SREF(timespec);
SREF(timeval);
SREF(itimerval);
SREF(tms);
EREF(ITIMER_PROF);

#if defined(HAVE_SYS_TIMEX_H)
// From sys/timex.h
SREF(timex);
#endif

// From sys/types.h
TREF(pid_t);
TREF(off_t);
TREF(loff_t);
TREF(size_t);
TREF(ssize_t);
TREF(mode_t);
TREF(dev_t);
TREF(time_t);

// From sys/ucontext.h
TREF(ucontext_t);

#if defined(HAVE_SYS_USER_H)
// From sys/user.h
SREF(user_regs_struct);
#endif

#if defined(HAVE_SYS_UTSNAME_H)
// From sys/utsname.h
SREF(utsname);
#endif

// From termios.h
SREF(termios);

// From uio.h
SREF(iovec);

// From utime.h
SREF(utimbuf);

// From unistd.h
EREF(_PC_NAME_MAX);
EREF(_SC_GETPW_R_SIZE_MAX);

#endif // clang
