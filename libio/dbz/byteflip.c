#include <stdio.h>

#define	MAXWORD	32

int
main(argc, argv)
int argc;
char *argv[];
{
	register int len;
	int inmap[MAXWORD];
	int outmap[MAXWORD];
	char in[MAXWORD];
	char out[MAXWORD];
	register int i;
	register int a;

	a = 1;
	len = atoi(argv[a++]);
	if (len > MAXWORD)
		abort();	/* kind of drastic... */
	for (i = 0; i < len; i++)
		inmap[i] = atoi(argv[a++]);
	if (atoi(argv[a++]) != len)
		abort();
	for (i = 0; i < len; i++)
		outmap[i] = atoi(argv[a++]);

	while (fread(in, 1, len, stdin) == len) {
		for (i = 0; i < len; i++)
			out[outmap[i]] = in[inmap[i]];
		fwrite(out, 1, len, stdout);
	}
#ifdef DBZ_FINISH
	DBZ_FINISH;
#endif
	exit(0);
}
