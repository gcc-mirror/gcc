#include <libio.h>

/* These emulate stdio functionality, but with a different name
   (_IO_ungetc instead of ungetc), and using _IO_FILE instead of FILE. */

#ifdef __cplusplus
extern "C" {
#endif

extern int _IO_fclose __P((_IO_FILE*));
extern _IO_FILE *_IO_fdopen __P((int, const char*));
extern int _IO_fflush __P((_IO_FILE*));
extern int _IO_fgetpos __P((_IO_FILE*, _IO_fpos_t*));
extern char* _IO_fgets __P((char*, int, _IO_FILE*));
extern _IO_FILE *_IO_fopen __P((const char*, const char*));
extern int _IO_fprintf __P((_IO_FILE*, const char*, ...));
extern int _IO_fputs __P((const char*, _IO_FILE*));
extern int _IO_fsetpos __P((_IO_FILE*, const _IO_fpos_t *));
extern long int _IO_ftell __P((_IO_FILE*));
extern _IO_size_t _IO_fread __P((void*, _IO_size_t, _IO_size_t, _IO_FILE*));
extern _IO_size_t _IO_fwrite __P((const void*,
				      _IO_size_t, _IO_size_t, _IO_FILE*));
extern char* _IO_gets __P((char*));
extern void _IO_perror __P((const char*));
extern int _IO_printf __P((const char*, ...));
extern int _IO_puts __P((const char*));
extern int _IO_scanf __P((const char*, ...));
extern void _IO_setbuffer __P((_IO_FILE *, char*, _IO_size_t));
extern int _IO_setvbuf __P((_IO_FILE*, char*, int, _IO_size_t));
extern int _IO_sscanf __P((const char*, const char*, ...));
extern int _IO_sprintf __P((char *, const char*, ...));
extern int _IO_ungetc __P((int, _IO_FILE*));
extern int _IO_vsscanf __P((const char *, const char *, _IO_va_list));
extern int _IO_vsprintf __P((char*, const char*, _IO_va_list));

struct obstack;
extern int _IO_obstack_vprintf __P ((struct obstack *, const char *,
                                    _IO_va_list));
extern int _IO_obstack_printf __P ((struct obstack *, const char *, ...));
#ifndef _IO_pos_BAD
# if defined(_G_IO_IO_FILE_VERSION) && _G_IO_IO_FILE_VERSION == 0x20001
#  define _IO_pos_BAD ((_IO_off64_t) -1)
# else
#  define _IO_pos_BAD ((_IO_off_t) -1)
# endif
#endif
#define _IO_clearerr(FP) ((FP)->_flags &= ~(_IO_ERR_SEEN|_IO_EOF_SEEN))
#define _IO_fseek(__fp, __offset, __whence) \
  (_IO_seekoff(__fp, __offset, __whence, _IOS_INPUT|_IOS_OUTPUT) == _IO_pos_BAD ? EOF : 0)
#define _IO_rewind(FILE) (void)_IO_seekoff(FILE, 0, 0, _IOS_INPUT|_IOS_OUTPUT)
#define _IO_vprintf(FORMAT, ARGS) _IO_vfprintf(_IO_stdout, FORMAT, ARGS)
#if _G_IO_IO_FILE_VERSION == 0x20001
#define _IO_freopen(FILENAME, MODE, FP) \
  (_IO_file_close_it(FP), _IO_file_fopen(FP, FILENAME, MODE, 0))
#else
#define _IO_freopen(FILENAME, MODE, FP) \
  (_IO_file_close_it(FP), _IO_file_fopen(FP, FILENAME, MODE))
#endif
#define _IO_fileno(FP) ((FP)->_fileno)
extern _IO_FILE* _IO_popen __P((const char*, const char*));
#define _IO_pclose _IO_fclose
#define _IO_setbuf(_FP, _BUF) _IO_setbuffer(_FP, _BUF, _IO_BUFSIZ)
#define _IO_setlinebuf(_FP) _IO_setvbuf(_FP, NULL, 1, 0)

#ifdef __cplusplus
}
#endif
