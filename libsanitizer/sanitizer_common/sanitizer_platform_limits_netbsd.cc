//===-- sanitizer_platform_limits_netbsd.cc -------------------------------===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is a part of Sanitizer common code.
//
// Sizes and layouts of platform-specific NetBSD data structures.
//===----------------------------------------------------------------------===//

#include "sanitizer_platform.h"

#if SANITIZER_NETBSD
#include <arpa/inet.h>
#include <dirent.h>
#include <glob.h>
#include <grp.h>
#include <ifaddrs.h>
#include <limits.h>
#include <link_elf.h>
#include <net/if.h>
#include <net/if_ether.h>
#include <net/ppp_defs.h>
#include <net/route.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/ip_mroute.h>
#include <poll.h>
#include <pthread.h>
#include <pwd.h>
#include <semaphore.h>
#include <signal.h>
#include <stddef.h>
#include <sys/filio.h>
#include <sys/ipc.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/mqueue.h>
#include <sys/msg.h>
#include <sys/mtio.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/shm.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/sockio.h>
#include <sys/soundcard.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/times.h>
#include <sys/timespec.h>
#include <sys/timex.h>
#include <sys/types.h>
#include <sys/ucontext.h>
#include <sys/utsname.h>
#include <term.h>
#include <termios.h>
#include <time.h>
#include <utime.h>
#include <utmp.h>
#include <utmpx.h>
#include <wchar.h>
#include <wordexp.h>

// Include these after system headers to avoid name clashes and ambiguities.
#include "sanitizer_internal_defs.h"
#include "sanitizer_platform_limits_netbsd.h"

namespace __sanitizer {
unsigned struct_utsname_sz = sizeof(struct utsname);
unsigned struct_stat_sz = sizeof(struct stat);
unsigned struct_rusage_sz = sizeof(struct rusage);
unsigned struct_tm_sz = sizeof(struct tm);
unsigned struct_passwd_sz = sizeof(struct passwd);
unsigned struct_group_sz = sizeof(struct group);
unsigned siginfo_t_sz = sizeof(siginfo_t);
unsigned struct_sigaction_sz = sizeof(struct sigaction);
unsigned struct_itimerval_sz = sizeof(struct itimerval);
unsigned pthread_t_sz = sizeof(pthread_t);
unsigned pthread_cond_t_sz = sizeof(pthread_cond_t);
unsigned pid_t_sz = sizeof(pid_t);
unsigned timeval_sz = sizeof(timeval);
unsigned uid_t_sz = sizeof(uid_t);
unsigned gid_t_sz = sizeof(gid_t);
unsigned mbstate_t_sz = sizeof(mbstate_t);
unsigned sigset_t_sz = sizeof(sigset_t);
unsigned struct_timezone_sz = sizeof(struct timezone);
unsigned struct_tms_sz = sizeof(struct tms);
unsigned struct_sigevent_sz = sizeof(struct sigevent);
unsigned struct_sched_param_sz = sizeof(struct sched_param);
unsigned struct_sockaddr_sz = sizeof(struct sockaddr);
unsigned ucontext_t_sz = sizeof(ucontext_t);
unsigned struct_rlimit_sz = sizeof(struct rlimit);
unsigned struct_timespec_sz = sizeof(struct timespec);
unsigned struct_utimbuf_sz = sizeof(struct utimbuf);
unsigned struct_itimerspec_sz = sizeof(struct itimerspec);
unsigned struct_timex_sz = sizeof(struct timex);
unsigned struct_msqid_ds_sz = sizeof(struct msqid_ds);
unsigned struct_mq_attr_sz = sizeof(struct mq_attr);
unsigned struct_statvfs_sz = sizeof(struct statvfs);

uptr sig_ign = (uptr)SIG_IGN;
uptr sig_dfl = (uptr)SIG_DFL;
uptr sa_siginfo = (uptr)SA_SIGINFO;

int shmctl_ipc_stat = (int)IPC_STAT;

unsigned struct_utmp_sz = sizeof(struct utmp);
unsigned struct_utmpx_sz = sizeof(struct utmpx);

int map_fixed = MAP_FIXED;

int af_inet = (int)AF_INET;
int af_inet6 = (int)AF_INET6;

uptr __sanitizer_in_addr_sz(int af) {
  if (af == AF_INET)
    return sizeof(struct in_addr);
  else if (af == AF_INET6)
    return sizeof(struct in6_addr);
  else
    return 0;
}

int glob_nomatch = GLOB_NOMATCH;
int glob_altdirfunc = GLOB_ALTDIRFUNC;

unsigned path_max = PATH_MAX;

// ioctl arguments
unsigned struct_ifreq_sz = sizeof(struct ifreq);
unsigned struct_termios_sz = sizeof(struct termios);
unsigned struct_winsize_sz = sizeof(struct winsize);
unsigned struct_mtget_sz = sizeof(struct mtget);
unsigned struct_mtop_sz = sizeof(struct mtop);
unsigned struct_audio_buf_info_sz = sizeof(struct audio_buf_info);
unsigned struct_ppp_stats_sz = sizeof(struct ppp_stats);
unsigned struct_sioc_sg_req_sz = sizeof(struct sioc_sg_req);
unsigned struct_sioc_vif_req_sz = sizeof(struct sioc_vif_req);

const unsigned IOCTL_NOT_PRESENT = 0;

unsigned IOCTL_FIOASYNC = FIOASYNC;
unsigned IOCTL_FIOCLEX = FIOCLEX;
unsigned IOCTL_FIOGETOWN = FIOGETOWN;
unsigned IOCTL_FIONBIO = FIONBIO;
unsigned IOCTL_FIONCLEX = FIONCLEX;
unsigned IOCTL_FIOSETOWN = FIOSETOWN;
unsigned IOCTL_SIOCADDMULTI = SIOCADDMULTI;
unsigned IOCTL_SIOCATMARK = SIOCATMARK;
unsigned IOCTL_SIOCDELMULTI = SIOCDELMULTI;
unsigned IOCTL_SIOCGIFADDR = SIOCGIFADDR;
unsigned IOCTL_SIOCGIFBRDADDR = SIOCGIFBRDADDR;
unsigned IOCTL_SIOCGIFCONF = SIOCGIFCONF;
unsigned IOCTL_SIOCGIFDSTADDR = SIOCGIFDSTADDR;
unsigned IOCTL_SIOCGIFFLAGS = SIOCGIFFLAGS;
unsigned IOCTL_SIOCGIFMETRIC = SIOCGIFMETRIC;
unsigned IOCTL_SIOCGIFMTU = SIOCGIFMTU;
unsigned IOCTL_SIOCGIFNETMASK = SIOCGIFNETMASK;
unsigned IOCTL_SIOCGPGRP = SIOCGPGRP;
unsigned IOCTL_SIOCSIFADDR = SIOCSIFADDR;
unsigned IOCTL_SIOCSIFBRDADDR = SIOCSIFBRDADDR;
unsigned IOCTL_SIOCSIFDSTADDR = SIOCSIFDSTADDR;
unsigned IOCTL_SIOCSIFFLAGS = SIOCSIFFLAGS;
unsigned IOCTL_SIOCSIFMETRIC = SIOCSIFMETRIC;
unsigned IOCTL_SIOCSIFMTU = SIOCSIFMTU;
unsigned IOCTL_SIOCSIFNETMASK = SIOCSIFNETMASK;
unsigned IOCTL_SIOCSPGRP = SIOCSPGRP;
unsigned IOCTL_TIOCCONS = TIOCCONS;
unsigned IOCTL_TIOCEXCL = TIOCEXCL;
unsigned IOCTL_TIOCGETD = TIOCGETD;
unsigned IOCTL_TIOCGPGRP = TIOCGPGRP;
unsigned IOCTL_TIOCGWINSZ = TIOCGWINSZ;
unsigned IOCTL_TIOCMBIC = TIOCMBIC;
unsigned IOCTL_TIOCMBIS = TIOCMBIS;
unsigned IOCTL_TIOCMGET = TIOCMGET;
unsigned IOCTL_TIOCMSET = TIOCMSET;
unsigned IOCTL_TIOCNOTTY = TIOCNOTTY;
unsigned IOCTL_TIOCNXCL = TIOCNXCL;
unsigned IOCTL_TIOCOUTQ = TIOCOUTQ;
unsigned IOCTL_TIOCPKT = TIOCPKT;
unsigned IOCTL_TIOCSCTTY = TIOCSCTTY;
unsigned IOCTL_TIOCSETD = TIOCSETD;
unsigned IOCTL_TIOCSPGRP = TIOCSPGRP;
unsigned IOCTL_TIOCSTI = TIOCSTI;
unsigned IOCTL_TIOCSWINSZ = TIOCSWINSZ;
unsigned IOCTL_SIOCGETSGCNT = SIOCGETSGCNT;
unsigned IOCTL_SIOCGETVIFCNT = SIOCGETVIFCNT;

const int si_SEGV_MAPERR = SEGV_MAPERR;
const int si_SEGV_ACCERR = SEGV_ACCERR;
}  // namespace __sanitizer

using namespace __sanitizer;

COMPILER_CHECK(sizeof(__sanitizer_pthread_attr_t) >= sizeof(pthread_attr_t));

COMPILER_CHECK(sizeof(socklen_t) == sizeof(unsigned));
CHECK_TYPE_SIZE(pthread_key_t);

// There are more undocumented fields in dl_phdr_info that we are not interested
// in.
COMPILER_CHECK(sizeof(__sanitizer_dl_phdr_info) <= sizeof(dl_phdr_info));
CHECK_SIZE_AND_OFFSET(dl_phdr_info, dlpi_addr);
CHECK_SIZE_AND_OFFSET(dl_phdr_info, dlpi_name);
CHECK_SIZE_AND_OFFSET(dl_phdr_info, dlpi_phdr);
CHECK_SIZE_AND_OFFSET(dl_phdr_info, dlpi_phnum);

CHECK_TYPE_SIZE(glob_t);
CHECK_SIZE_AND_OFFSET(glob_t, gl_pathc);
CHECK_SIZE_AND_OFFSET(glob_t, gl_pathv);
CHECK_SIZE_AND_OFFSET(glob_t, gl_offs);
CHECK_SIZE_AND_OFFSET(glob_t, gl_flags);
CHECK_SIZE_AND_OFFSET(glob_t, gl_closedir);
CHECK_SIZE_AND_OFFSET(glob_t, gl_readdir);
CHECK_SIZE_AND_OFFSET(glob_t, gl_opendir);
CHECK_SIZE_AND_OFFSET(glob_t, gl_lstat);
CHECK_SIZE_AND_OFFSET(glob_t, gl_stat);

CHECK_TYPE_SIZE(addrinfo);
CHECK_SIZE_AND_OFFSET(addrinfo, ai_flags);
CHECK_SIZE_AND_OFFSET(addrinfo, ai_family);
CHECK_SIZE_AND_OFFSET(addrinfo, ai_socktype);
CHECK_SIZE_AND_OFFSET(addrinfo, ai_protocol);
CHECK_SIZE_AND_OFFSET(addrinfo, ai_protocol);
CHECK_SIZE_AND_OFFSET(addrinfo, ai_addrlen);
CHECK_SIZE_AND_OFFSET(addrinfo, ai_canonname);
CHECK_SIZE_AND_OFFSET(addrinfo, ai_addr);

CHECK_TYPE_SIZE(hostent);
CHECK_SIZE_AND_OFFSET(hostent, h_name);
CHECK_SIZE_AND_OFFSET(hostent, h_aliases);
CHECK_SIZE_AND_OFFSET(hostent, h_addrtype);
CHECK_SIZE_AND_OFFSET(hostent, h_length);
CHECK_SIZE_AND_OFFSET(hostent, h_addr_list);

CHECK_TYPE_SIZE(iovec);
CHECK_SIZE_AND_OFFSET(iovec, iov_base);
CHECK_SIZE_AND_OFFSET(iovec, iov_len);

CHECK_TYPE_SIZE(msghdr);
CHECK_SIZE_AND_OFFSET(msghdr, msg_name);
CHECK_SIZE_AND_OFFSET(msghdr, msg_namelen);
CHECK_SIZE_AND_OFFSET(msghdr, msg_iov);
CHECK_SIZE_AND_OFFSET(msghdr, msg_iovlen);
CHECK_SIZE_AND_OFFSET(msghdr, msg_control);
CHECK_SIZE_AND_OFFSET(msghdr, msg_controllen);
CHECK_SIZE_AND_OFFSET(msghdr, msg_flags);

CHECK_TYPE_SIZE(cmsghdr);
CHECK_SIZE_AND_OFFSET(cmsghdr, cmsg_len);
CHECK_SIZE_AND_OFFSET(cmsghdr, cmsg_level);
CHECK_SIZE_AND_OFFSET(cmsghdr, cmsg_type);

COMPILER_CHECK(sizeof(__sanitizer_dirent) <= sizeof(dirent));
CHECK_SIZE_AND_OFFSET(dirent, d_fileno);
CHECK_SIZE_AND_OFFSET(dirent, d_reclen);

CHECK_TYPE_SIZE(ifconf);
CHECK_SIZE_AND_OFFSET(ifconf, ifc_len);
CHECK_SIZE_AND_OFFSET(ifconf, ifc_ifcu);

CHECK_TYPE_SIZE(pollfd);
CHECK_SIZE_AND_OFFSET(pollfd, fd);
CHECK_SIZE_AND_OFFSET(pollfd, events);
CHECK_SIZE_AND_OFFSET(pollfd, revents);

CHECK_TYPE_SIZE(nfds_t);

CHECK_TYPE_SIZE(sigset_t);

COMPILER_CHECK(sizeof(__sanitizer_sigaction) == sizeof(struct sigaction));
// Can't write checks for sa_handler and sa_sigaction due to them being
// preprocessor macros.
CHECK_STRUCT_SIZE_AND_OFFSET(sigaction, sa_mask);

CHECK_TYPE_SIZE(wordexp_t);
CHECK_SIZE_AND_OFFSET(wordexp_t, we_wordc);
CHECK_SIZE_AND_OFFSET(wordexp_t, we_wordv);
CHECK_SIZE_AND_OFFSET(wordexp_t, we_offs);

CHECK_TYPE_SIZE(tm);
CHECK_SIZE_AND_OFFSET(tm, tm_sec);
CHECK_SIZE_AND_OFFSET(tm, tm_min);
CHECK_SIZE_AND_OFFSET(tm, tm_hour);
CHECK_SIZE_AND_OFFSET(tm, tm_mday);
CHECK_SIZE_AND_OFFSET(tm, tm_mon);
CHECK_SIZE_AND_OFFSET(tm, tm_year);
CHECK_SIZE_AND_OFFSET(tm, tm_wday);
CHECK_SIZE_AND_OFFSET(tm, tm_yday);
CHECK_SIZE_AND_OFFSET(tm, tm_isdst);
CHECK_SIZE_AND_OFFSET(tm, tm_gmtoff);
CHECK_SIZE_AND_OFFSET(tm, tm_zone);

CHECK_TYPE_SIZE(ether_addr);

CHECK_TYPE_SIZE(ipc_perm);
CHECK_SIZE_AND_OFFSET(ipc_perm, _key);
CHECK_SIZE_AND_OFFSET(ipc_perm, _seq);
CHECK_SIZE_AND_OFFSET(ipc_perm, uid);
CHECK_SIZE_AND_OFFSET(ipc_perm, gid);
CHECK_SIZE_AND_OFFSET(ipc_perm, cuid);
CHECK_SIZE_AND_OFFSET(ipc_perm, cgid);
CHECK_SIZE_AND_OFFSET(ipc_perm, mode);

CHECK_TYPE_SIZE(shmid_ds);
CHECK_SIZE_AND_OFFSET(shmid_ds, shm_perm);
CHECK_SIZE_AND_OFFSET(shmid_ds, shm_segsz);
CHECK_SIZE_AND_OFFSET(shmid_ds, shm_atime);
CHECK_SIZE_AND_OFFSET(shmid_ds, shm_dtime);
CHECK_SIZE_AND_OFFSET(shmid_ds, shm_ctime);
CHECK_SIZE_AND_OFFSET(shmid_ds, shm_cpid);
CHECK_SIZE_AND_OFFSET(shmid_ds, shm_lpid);
CHECK_SIZE_AND_OFFSET(shmid_ds, shm_nattch);

CHECK_TYPE_SIZE(clock_t);

CHECK_TYPE_SIZE(ifaddrs);
CHECK_SIZE_AND_OFFSET(ifaddrs, ifa_next);
CHECK_SIZE_AND_OFFSET(ifaddrs, ifa_name);
CHECK_SIZE_AND_OFFSET(ifaddrs, ifa_addr);
CHECK_SIZE_AND_OFFSET(ifaddrs, ifa_netmask);
// Compare against the union, because we can't reach into the union in a
// compliant way.
#ifdef ifa_dstaddr
#undef ifa_dstaddr
#endif
CHECK_SIZE_AND_OFFSET(ifaddrs, ifa_dstaddr);
CHECK_SIZE_AND_OFFSET(ifaddrs, ifa_data);

CHECK_TYPE_SIZE(timeb);
CHECK_SIZE_AND_OFFSET(timeb, time);
CHECK_SIZE_AND_OFFSET(timeb, millitm);
CHECK_SIZE_AND_OFFSET(timeb, timezone);
CHECK_SIZE_AND_OFFSET(timeb, dstflag);

CHECK_TYPE_SIZE(passwd);
CHECK_SIZE_AND_OFFSET(passwd, pw_name);
CHECK_SIZE_AND_OFFSET(passwd, pw_passwd);
CHECK_SIZE_AND_OFFSET(passwd, pw_uid);
CHECK_SIZE_AND_OFFSET(passwd, pw_gid);
CHECK_SIZE_AND_OFFSET(passwd, pw_dir);
CHECK_SIZE_AND_OFFSET(passwd, pw_shell);

CHECK_SIZE_AND_OFFSET(passwd, pw_gecos);

CHECK_TYPE_SIZE(group);
CHECK_SIZE_AND_OFFSET(group, gr_name);
CHECK_SIZE_AND_OFFSET(group, gr_passwd);
CHECK_SIZE_AND_OFFSET(group, gr_gid);
CHECK_SIZE_AND_OFFSET(group, gr_mem);

#endif  // SANITIZER_NETBSD
