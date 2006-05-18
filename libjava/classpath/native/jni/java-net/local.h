#ifndef __LOCAL_H__
#define __LOCAL_H__

#ifdef __cplusplus
extern "C"
{
#endif
#define __EMACSEN__ }

extern const char *local_error (void);
extern int local_create (int);
extern int local_bind (int, const char *);
extern int local_listen (int, int);
extern int local_accept (int, char *);
extern int local_available (int);
extern int local_close (int);
extern int local_shutdown_input (int);
extern int local_shutdown_output (int);
extern int local_connect (int, char *);
extern int local_unlink (char *);
extern int local_read (int, void *, int);
extern int local_write (int, void *, int);

#ifdef __cplusplus
}
#endif

#endif /* __LOCAL_H__ */
