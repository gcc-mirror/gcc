#include <stdio.h>
#include <signal.h>

#ifndef SIGIOT
#ifdef SIGABRT
#define SIGIOT SIGABRT
#endif
#endif

#ifdef KR_headers
void sig_die(s, kill) register char *s; int kill;
#else
#include <stdlib.h>
#ifdef __cplusplus
extern "C" {
#endif
 extern void f_exit(void);

void sig_die(register char *s, int kill)
#endif
{
	/* print error message, then clear buffers */
	fprintf(stderr, "%s\n", s);

	if(kill)
		{
		fflush(stderr);
		f_exit();
		fflush(stderr);
		/* now get a core */
#ifdef SIGIOT
		signal(SIGIOT, SIG_DFL);
#endif
		abort();
		}
	else {
#ifdef NO_ONEXIT
		f_exit();
#endif
		exit(1);
		}
	}
#ifdef __cplusplus
}
#endif
