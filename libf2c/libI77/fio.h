#include <stdio.h>
#include <errno.h>
#ifndef NULL
/* ANSI C */
#include <stddef.h>
#endif
#ifdef STDC_HEADERS
#include <string.h>
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#if defined (MSDOS) && !defined (GO32)
#ifndef NON_UNIX_STDIO
#define NON_UNIX_STDIO
#endif
#endif

#ifdef UIOLEN_int
typedef int uiolen;
#else
typedef long uiolen;
#endif

/*units*/
typedef struct
{	FILE *ufd;	/*0=unconnected*/
	char *ufnm;
#if !(defined (MSDOS) && !defined (GO32))
	long uinode;
	int udev;
#endif
	int url;	/*0=sequential*/
	flag useek;	/*true=can backspace, use dir, ...*/
	flag ufmt;
	flag urw;	/* (1 for can read) | (2 for can write) */
	flag ublnk;
	flag uend;
	flag uwrt;	/*last io was write*/
	flag uscrtch;
} unit;

extern int f__init;
extern cilist *f__elist;	/*active external io list*/
extern flag f__reading,f__external,f__sequential,f__formatted;
#undef Void
#ifdef KR_headers
#define Void /*void*/
extern int (*f__getn)();	/* for formatted input */
extern void (*f__putn)();	/* for formatted output */
extern void x_putc();
extern long f__inode();
extern VOID sig_die();
extern int (*f__donewrec)(), t_putc(), x_wSL();
extern int c_sfe(), err__fl(), xrd_SL(), f__putbuf();
#else
#define Void void
#ifdef __cplusplus
extern "C" {
#endif
extern int (*f__getn)(void);	/* for formatted input */
extern void (*f__putn)(int);	/* for formatted output */
extern void x_putc(int);
extern long f__inode(char*,int*);
extern void sig_die(char*,int);
extern void f__fatal(int,char*);
extern int t_runc(alist*);
extern int f__nowreading(unit*), f__nowwriting(unit*);
extern int fk_open(int,int,ftnint);
extern int en_fio(void);
extern void f_init(void);
extern int (*f__donewrec)(void), t_putc(int), x_wSL(void);
extern void b_char(char*,char*,ftnlen), g_char(char*,ftnlen,char*);
extern int c_sfe(cilist*), z_rnew(void);
extern int isatty(int);
extern int err__fl(int,int,char*);
extern int xrd_SL(void);
extern int f__putbuf(int);
#ifdef __cplusplus
	}
#endif
#endif
extern int (*f__doend)(Void);
extern FILE *f__cf;	/*current file*/
extern unit *f__curunit;	/*current unit*/
extern unit f__units[];
#define err(f,m,s) do {if(f) {f__init &= ~2; errno= m;} else f__fatal(m,s); return(m);} while(0)
#define errfl(f,m,s) do {return err__fl((int)f,m,s);} while(0)

/*Table sizes*/
#define MXUNIT 100

extern int f__recpos;	/*position in current record*/
extern int f__cursor;	/* offset to move to */
extern int f__hiwater;	/* so TL doesn't confuse us */

#define WRITE	1
#define READ	2
#define SEQ	3
#define DIR	4
#define FMT	5
#define UNF	6
#define EXT	7
#define INT	8

#define buf_end(x) (x->_flag & _IONBF ? x->_ptr : x->_base + BUFSIZ)
