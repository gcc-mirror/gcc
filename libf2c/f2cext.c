/* Copyright (C) 1997 Free Software Foundation, Inc.
This file is part of GNU Fortran run-time library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include <f2c.h>
typedef void *sig_proc;	/* For now, this will have to do. */

#ifdef Labort
int abort_ (void) {
    extern int G77_abort_0 (void);
    return G77_abort_0 ();
}
#endif

#ifdef Lderf
double derf_ (doublereal *x) {
    extern double G77_derf_0 (doublereal *x);
    return G77_derf_0 (x);
}
#endif

#ifdef Lderfc
double derfc_ (doublereal *x) {
    extern double G77_derfc_0 (doublereal *x);
    return G77_derfc_0 (x);
}
#endif

#ifdef Lef1asc
int ef1asc_ (ftnint *a, ftnlen *la, ftnint *b, ftnlen *lb) {
    extern int G77_ef1asc_0 (ftnint *a, ftnlen *la, ftnint *b, ftnlen *lb);
    return G77_ef1asc_0 (a, la, b, lb);
}
#endif

#ifdef Lef1cmc
integer ef1cmc_ (ftnint *a, ftnlen *la, ftnint *b, ftnlen *lb) {
    extern integer G77_ef1cmc_0 (ftnint *a, ftnlen *la, ftnint *b, ftnlen *lb);
    return G77_ef1cmc_0 (a, la, b, lb);
}
#endif

#ifdef Lerf
double erf_ (real *x) {
    extern double G77_erf_0 (real *x);
    return G77_erf_0 (x);
}
#endif

#ifdef Lerfc
double erfc_ (real *x) {
    extern double G77_erfc_0 (real *x);
    return G77_erfc_0 (x);
}
#endif

#ifdef Lexit
void exit_ (integer *rc) {
    extern void G77_exit_0 (integer *rc);
    G77_exit_0 (rc);
}
#endif

#ifdef Lgetarg
void getarg_ (ftnint *n, char *s, ftnlen ls) {
    extern void G77_getarg_0 (ftnint *n, char *s, ftnlen ls);
    G77_getarg_0 (n, s, ls);
}
#endif

#ifdef Lgetenv
void getenv_ (char *fname, char *value, ftnlen flen, ftnlen vlen) {
    extern void G77_getenv_0 (char *fname, char *value, ftnlen flen, ftnlen vlen);
    G77_getenv_0 (fname, value, flen, vlen);
}
#endif

#ifdef Liargc
ftnint iargc_ (void) {
    extern ftnint G77_iargc_0 (void);
    return G77_iargc_0 ();
}
#endif

#ifdef Lsignal
void *signal_ (integer *sigp, sig_proc proc) {
    extern void *G77_signal_0 (integer *sigp, sig_proc proc);
    return G77_signal_0 (sigp, proc);
}
#endif

#ifdef Lsystem
integer system_ (char *s, ftnlen n) {
    extern integer G77_system_0 (char *s, ftnlen n);
    return G77_system_0 (s, n);
}
#endif

#ifdef Lflush
int flush_ (void) {
    extern int G77_flush_0 (void);
    return G77_flush_0 ();
}
#endif

#ifdef Lftell
integer ftell_ (integer *Unit) {
    extern integer G77_ftell_0 (integer *Unit);
    return G77_ftell_0 (Unit);
}
#endif

#ifdef Lfseek
integer fseek_ (integer *Unit, integer *offset, integer *xwhence) {
    extern integer G77_fseek_0 (integer *Unit, integer *offset, integer *xwhence);
    return G77_fseek_0 (Unit, offset, xwhence);
}
#endif

#ifdef Laccess
integer access_ (const char *name, const char *mode, ftnlen Lname, ftnlen Lmode) {
    extern integer G77_access_0 (const char *name, const char *mode, ftnlen Lname, ftnlen Lmode);
    return G77_access_0 (name, mode, Lname, Lmode);
}
#endif

#ifdef Lalarm
integer alarm_ (integer *seconds, sig_proc proc, integer *status) {
    extern integer G77_alarm_0 (integer *seconds, sig_proc proc);
    return G77_alarm_0 (seconds, proc);
}
#endif

#ifdef Lbesj0
double besj0_ (const real *x) {
    return j0 (*x);
}
#endif

#ifdef Lbesj1
double besj1_ (const real *x) {
    return j1 (*x);
}
#endif

#ifdef Lbesjn
double besjn_ (const integer *n, real *x) {
    return jn (*n, *x);
}
#endif

#ifdef Lbesy0
double besy0_ (const real *x) {
    return y0 (*x);
}
#endif

#ifdef Lbesy1
double besy1_ (const real *x) {
    return y1 (*x);
}
#endif

#ifdef Lbesyn
double besyn_ (const integer *n, real *x) {
    return yn (*n, *x);
}
#endif

#ifdef Lchdir
integer chdir_ (const char *name, const ftnlen Lname) {
    extern integer G77_chdir_0 (const char *name, const ftnlen Lname);
    return G77_chdir_0 (name, Lname);
}
#endif

#ifdef Lchmod
integer chmod_ (const char *name, const char *mode, const ftnlen Lname, const ftnlen Lmode) {
    extern integer G77_chmod_0 (const char *name, const char *mode, const ftnlen Lname, const ftnlen Lmode);
    return G77_chmod_0 (name, mode, Lname, Lmode);
}
#endif

#ifdef Lctime
void ctime_ (char *chtime, const ftnlen Lchtime, longint *xstime) {
    extern void G77_ctime_0 (char *chtime, const ftnlen Lchtime, longint *xstime);
    G77_ctime_0 (chtime, Lchtime, xstime);
}
#endif

#ifdef Ldate
int date_ (char *buf, ftnlen buf_len) {
    extern int G77_date_0 (char *buf, ftnlen buf_len);
    return G77_date_0 (buf, buf_len);
}
#endif

#ifdef Ldbesj0
double dbesj0_ (const double *x) {
    return j0 (*x);
}
#endif

#ifdef Ldbesj1
double dbesj1_ (const double *x) {
    return j1 (*x);
}
#endif

#ifdef Ldbesjn
double dbesjn_ (const integer *n, double *x) {
    return jn (*n, *x);
}
#endif

#ifdef Ldbesy0
double dbesy0_ (const double *x) {
    return y0 (*x);
}
#endif

#ifdef Ldbesy1
double dbesy1_ (const double *x) {
    return y1 (*x);
}
#endif

#ifdef Ldbesyn
double dbesyn_ (const integer *n, double *x) {
    return yn (*n, *x);
}
#endif

#ifdef Ldtime
double dtime_ (real tarray[2]) {
    extern double G77_dtime_0 (real tarray[2]);
    return G77_dtime_0 (tarray);
}
#endif

#ifdef Letime
double etime_ (real tarray[2]) {
    extern double G77_etime_0 (real tarray[2]);
    return G77_etime_0 (tarray);
}
#endif

#ifdef Lfdate
void fdate_ (char *ret_val, ftnlen ret_val_len) {
    extern void G77_fdate_0 (char *ret_val, ftnlen ret_val_len);
    G77_fdate_0 (ret_val, ret_val_len);
}
#endif

#ifdef Lfgetc
integer fgetc_ (const integer *lunit, char *c, ftnlen Lc) {
    extern integer G77_fgetc_0 (const integer *lunit, char *c, ftnlen Lc);
    return G77_fgetc_0 (lunit, c, Lc);
}
#endif

#ifdef Lfget
integer fget_ (char *c, const ftnlen Lc) {
    extern integer G77_fget_0 (char *c, const ftnlen Lc);
    return G77_fget_0 (c, Lc);
}
#endif

#ifdef Lflush1
int flush1_ (const integer *lunit) {
    extern int G77_flush1_0 (const integer *lunit);
    return G77_flush1_0 (lunit);
}
#endif

#ifdef Lfnum
integer fnum_ (integer *lunit) {
    extern integer G77_fnum_0 (integer *lunit);
    return G77_fnum_0 (lunit);
}
#endif

#ifdef Lfputc
integer fputc_ (const integer *lunit, const char *c, const ftnlen Lc) {
    extern integer G77_fputc_0 (const integer *lunit, const char *c, const ftnlen Lc);
    return G77_fputc_0 (lunit, c, Lc);
}
#endif

#ifdef Lfput
integer fput_ (const char *c, const ftnlen Lc) {
    extern integer G77_fput_0 (const char *c, const ftnlen Lc);
    return G77_fput_0 (c, Lc);
}
#endif

#ifdef Lfstat
integer fstat_ (const integer *lunit, integer statb[13]) {
    extern integer G77_fstat_0 (const integer *lunit, integer statb[13]);
    return G77_fstat_0 (lunit, statb);
}
#endif

#ifdef Lgerror
int gerror_ (char *str, ftnlen Lstr) {
    extern int G77_gerror_0 (char *str, ftnlen Lstr);
    return G77_gerror_0 (str,  Lstr);
}
#endif

#ifdef Lgetcwd
integer getcwd_ (char *str, const ftnlen Lstr) {
    extern integer G77_getcwd_0 (char *str, const ftnlen Lstr);
    return G77_getcwd_0 (str, Lstr);
}
#endif

#ifdef Lgetgid
integer getgid_ (void) {
    extern integer G77_getgid_0 (void);
    return G77_getgid_0 ();
}
#endif

#ifdef Lgetlog
int getlog_ (char *str, const ftnlen Lstr) {
    extern int G77_getlog_0 (char *str, const ftnlen Lstr);
    return G77_getlog_0 (str, Lstr);
}
#endif

#ifdef Lgetpid
integer getpid_ (void) {
    extern integer G77_getpid_0 (void);
    return G77_getpid_0 ();
}
#endif

#ifdef Lgetuid
integer getuid_ (void) {
    extern integer G77_getuid_0 (void);
    return G77_getuid_0 ();
}
#endif

#ifdef Lgmtime
int gmtime_ (const integer *stime, integer tarray[9]) {
    extern int G77_gmtime_0 (const integer *stime, integer tarray[9]);
    return G77_gmtime_0 (stime, tarray);
}
#endif

#ifdef Lhostnm
integer hostnm_ (char *name, ftnlen Lname) {
    extern integer G77_hostnm_0 (char *name, ftnlen Lname);
    return G77_hostnm_0 (name, Lname);
}
#endif

#ifdef Lidate
int idate_ (int iarray[3]) {
    extern int G77_idate_0 (int iarray[3]);
    return G77_idate_0 (iarray);
}
#endif

#ifdef Lierrno
integer ierrno_ (void) {
    extern integer G77_ierrno_0 (void);
    return G77_ierrno_0 ();
}
#endif

#ifdef Lirand
integer irand_ (integer *flag) {
    extern integer G77_irand_0 (integer *flag);
    return G77_irand_0 (flag);
}
#endif

#ifdef Lisatty
logical isatty_ (integer *lunit) {
    extern logical G77_isatty_0 (integer *lunit);
    return G77_isatty_0 (lunit);
}
#endif

#ifdef Litime
int itime_ (integer tarray[3]) {
    extern int G77_itime_0 (integer tarray[3]);
    return G77_itime_0 (tarray);
}
#endif

#ifdef Lkill
integer kill_ (const integer *pid, const integer *signum) {
    extern integer G77_kill_0 (const integer *pid, const integer *signum);
    return G77_kill_0 (pid, signum);
}
#endif

#ifdef Llink
integer link_ (const char *path1, const char *path2, const ftnlen Lpath1, const ftnlen Lpath2) {
    extern integer G77_link_0 (const char *path1, const char *path2, const ftnlen Lpath1, const ftnlen Lpath2);
    return G77_link_0 (path1, path2, Lpath1, Lpath2);
}
#endif

#ifdef Llnblnk
integer lnblnk_ (char *str, ftnlen str_len) {
    extern integer G77_lnblnk_0 (char *str, ftnlen str_len);
    return G77_lnblnk_0 (str, str_len);
}
#endif

#ifdef Llstat
integer lstat_ (const char *name, integer statb[13], const ftnlen Lname) {
    extern integer G77_lstat_0 (const char *name, integer statb[13], const ftnlen Lname);
    return G77_lstat_0 (name, statb, Lname);
}
#endif

#ifdef Lltime
int ltime_ (const integer *stime, integer tarray[9]) {
    extern int G77_ltime_0 (const integer *stime, integer tarray[9]);
    return G77_ltime_0 (stime, tarray);
}
#endif

#ifdef Lmclock
longint mclock_ (void) {
    extern longint G77_mclock_0 (void);
    return G77_mclock_0 ();
}
#endif

#ifdef Lperror
int perror_ (const char *str, const ftnlen Lstr) {
    extern int G77_perror_0 (const char *str, const ftnlen Lstr);
    return G77_perror_0 (str, Lstr);
}
#endif

#ifdef Lrand
double rand_ (integer *flag) {
    extern double G77_rand_0 (integer *flag);
    return G77_rand_0 (flag);
}
#endif

#ifdef Lrename
integer rename_ (const char *path1, const char *path2, const ftnlen Lpath1, const ftnlen Lpath2) {
    extern integer G77_rename_0 (const char *path1, const char *path2, const ftnlen Lpath1, const ftnlen Lpath2);
    return G77_rename_0 (path1, path2, Lpath1, Lpath2);
}
#endif

#ifdef Lsecnds
double secnds_ (real *r) {
    extern double G77_secnds_0 (real *r);
    return G77_secnds_0 (r);
}
#endif

#ifdef Lsecond
double second_ () {
    extern double G77_second_0 ();
    return G77_second_0 ();
}
#endif

#ifdef Lsleep
int sleep_ (const integer *seconds) {
    extern int G77_sleep_0 (const integer *seconds);
    return G77_sleep_0 (seconds);
}
#endif

#ifdef Lsrand
int srand_ (const integer *seed) {
    extern int G77_srand_0 (const integer *seed);
    return G77_srand_0 (seed);
}
#endif

#ifdef Lstat
integer stat_ (const char *name, integer statb[13], const ftnlen Lname) {
    extern integer G77_stat_0 (const char *name, integer statb[13], const ftnlen Lname);
    return G77_stat_0 (name, statb, Lname);
}
#endif

#ifdef Lsymlnk
integer symlnk_ (const char *path1, const char *path2, const ftnlen Lpath1, const ftnlen Lpath2) {
    extern integer G77_symlnk_0 (const char *path1, const char *path2, const ftnlen Lpath1, const ftnlen Lpath2);
    return G77_symlnk_0 (path1, path2, Lpath1, Lpath2);
}
#endif

#ifdef Ltime
longint time_ (void) {
    extern longint G77_time_0 (void);
    return G77_time_0 ();
}
#endif

#ifdef Lttynam
void ttynam_ (char *ret_val, ftnlen ret_val_len, integer *lunit) {
    extern void G77_ttynam_0 (char *ret_val, ftnlen ret_val_len, integer *lunit);
    G77_ttynam_0 (ret_val, ret_val_len, lunit);
}
#endif

#ifdef Lumask
integer umask_ (integer *mask) {
    extern integer G77_umask_0 (integer *mask);
    return G77_umask_0 (mask);
}
#endif

#ifdef Lunlink
integer unlink_ (const char *str, const ftnlen Lstr) {
    extern integer G77_unlink_0 (const char *str, const ftnlen Lstr);
    return G77_unlink_0 (str, Lstr);
}
#endif

#ifdef Lvxtidt
int vxtidate_ (integer *m, integer *d, integer *y) {
    extern int G77_vxtidate_0 (integer *m, integer *d, integer *y);
    return G77_vxtidate_0 (m, d, y);
}
#endif

#ifdef Lvxttim
void vxttime_ (char chtime[8], const ftnlen Lchtime) {
    extern void G77_vxttime_0 (char chtime[8], const ftnlen Lchtime);
    G77_vxttime_0 (chtime, Lchtime);
}
#endif
