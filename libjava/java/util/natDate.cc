#if HAVE_CTIME_R
/* Use overload resolution to find out the signature of ctime_r.  */

  /* This is Posix ctime_r().  */
template <typename T_clock, typename T_buf, size_t buflen>
static inline char *
ctime_adaptor (char* (*ctime_r)(T_clock *clock, T_buf *buf),
	       time_t *clock, char (&buf)[buflen])
{
  return ctime_r(clock, buf);
}

/* This is an old-style ctime_r, used on IRIX 5.2.  */
template <typename T_clock, typename T_buf, typename T_buflen, size_t buflen>
static inline char *
ctime_adaptor (char* (*ctime_r)(T_clock *clock, T_buf *buf, T_buflen len),
	       time_t *clock, char (&buf)[buflen])
{
  return ctime_r(clock, buf, buflen);
}
#endif

  return JvNewStringLatin1 (ctime_adaptor (ctime_r, &t, buf));
