/* Set up the signal behavior.  */

#include <stdio.h>
#include "signal1.h"

#ifndef SIGIOT
#ifdef SIGABRT
#define SIGIOT SIGABRT
#endif
#endif

#ifndef KR_headers
#undef VOID
#include <stdlib.h>
#endif

#ifndef VOID
#define VOID void
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef KR_headers
extern VOID sig_die();
#define Int /* int */
#else
extern void sig_die(char*, int);
#define Int int
#endif

static VOID sigfdie(Sigarg)
{
Use_Sigarg;
sig_die("Floating Exception", 1);
}


static VOID sigidie(Sigarg)
{
Use_Sigarg;
sig_die("IOT Trap", 1);
}

#ifdef SIGQUIT
static VOID sigqdie(Sigarg)
{
Use_Sigarg;
sig_die("Quit signal", 1);
}
#endif


static VOID sigindie(Sigarg)
{
Use_Sigarg;
sig_die("Interrupt", 0);
}

static VOID sigtdie(Sigarg)
{
Use_Sigarg;
sig_die("Killed", 0);
}

#ifdef SIGTRAP
static VOID sigtrdie(Sigarg)
{
Use_Sigarg;
sig_die("Trace trap", 1);
}
#endif


#ifdef __cplusplus
	}
#endif

 void
f_setsig()
{
signal1(SIGFPE, sigfdie);	/* ignore underflow, enable overflow */
#ifdef SIGIOT
signal1(SIGIOT, sigidie);
#endif
#ifdef SIGTRAP
signal1(SIGTRAP, sigtrdie);
#endif
#ifdef SIGQUIT
if(signal1(SIGQUIT,sigqdie) == SIG_IGN)
	signal1(SIGQUIT, SIG_IGN);
#endif
if(signal1(SIGINT, sigindie) == SIG_IGN)
	signal1(SIGINT, SIG_IGN);
signal1(SIGTERM,sigtdie);

#ifdef pdp11
	ldfps(01200); /* detect overflow as an exception */
#endif
}
