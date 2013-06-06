/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/**
 * @file gupcr_utils.h
 * GUPC Runtime utility routines
 */

/**
 * @addtogroup GUPCUTILS GUPCR Utility Functions
 * @{
 */

#ifndef _GUPCR_UTILS_H_

//begin lib_utils_api
#define _GUPCR_UTILS_H_ 1

#ifndef __STRING
/** Return 'x' argument as string */
#define __STRING(x)     #x
#endif

/* Give up control of the CPU for a small time interval.  */
#ifdef _POSIX_PRIORITY_SCHEDULING
#define gupcr_yield_cpu() do { sched_yield(); } while (0)
#else
#define gupcr_yield_cpu() do { usleep(1000L); } while (0)
#endif

/* Give up control of the CPU for specified amount of time.  */
#define gupcr_cpu_delay(MICROSECS) do { usleep(MICROSECS); } while (0)

/* The definitions below use GCC's extensions to variadic macros.
 * See: http://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html
 * These extensions permit empty variadic arg lists.  */

#define gupcr_error(fmt, args...)					\
  gupcr_error_print ("%s: UPC error: " fmt "\n",			\
                    gupcr_get_pgm_name (), ##args)

#define gupcr_warn(fmt, args...)					\
  gupcr_warn_print ("%s: UPC warning: " fmt "\n",			\
                    gupcr_get_pgm_name (), ##args)

#define gupcr_info(fmt, args...)					\
  gupcr_info_print ("%s: UPC info: " fmt "\n",				\
                    gupcr_get_pgm_name (), ##args)

#define gupcr_fatal_error(fmt, args...)					\
  gupcr_abort_with_msg ("[%d] %s: %s:%d: %s: " fmt "\n",		\
               MYTHREAD, gupcr_get_pgm_name (),				\
	       __FILE__, __LINE__, __func__, ##args)

#ifdef GUPCR_HAVE_CHECKS
#define gupcr_assert(expr) (expr) ? (void)(0)				\
   : gupcr_fatal_error ("UPC runtime assertion `%s' failed",		\
                        __STRING(expr))
#else
#define gupcr_assert(expr)
#endif

#define gupcr_syscall(syscall, args)					\
    do									\
      {									\
        int status;							\
        status = syscall args;						\
	if (status < 0)					        	\
	  gupcr_fatal_error ("UPC runtime system call `%s' failed: %s",	\
	                     __STRING(syscall),				\
	                     gupcr_strerror ());			\
      }									\
    while (0)

#define gupcr_getcwd(path)						\
    do									\
      {									\
        path = getcwd (NULL, 0);					\
	if (!path)					        	\
	  gupcr_fatal_error ("UPC runtime `getcwd' failed: %s",		\
	                     gupcr_strerror ());			\
      }									\
    while (0)

#define gupcr_mkdir_unless_exists(dir)					\
    do									\
      {									\
        int status;							\
        status = mkdir (dir, 0755);					\
	if (status < 0 && errno != EEXIST)		        	\
	  gupcr_fatal_error ("UPC runtime `mkdir' of `%s' failed: %s",	\
	                     dir, __STRING(syscall));			\
      }									\
    while (0)

#define gupcr_malloc(ptr, size)						\
    do									\
      {									\
        ptr = malloc (size);						\
	if (!ptr)					        	\
	  gupcr_fatal_error ("UPC runtime malloc of %lld bytes failed",	\
	                     (long long int)size);			\
      }									\
    while (0)

#define gupcr_strdup(str, orig)						\
    do									\
      {									\
        if (!orig)							\
	  gupcr_fatal_error ("UPC runtime attempt to "			\
	                     "duplicate a null string");		\
        str = strdup (orig);						\
	if (!str)					        	\
	  gupcr_fatal_error ("UPC runtime strdup of %ld bytes failed",	\
	                     (long int)strlen (orig));			\
      }									\
    while (0)


#define gupcr_free(ptr)							\
    do									\
      {									\
        if (!ptr)							\
	  gupcr_fatal_error ("UPC runtime attempt to "			\
	                     "free a null pointer");			\
	free (ptr);							\
      }									\
    while (0)

#ifdef GUPCR_HAVE_DEBUG
#define gupcr_debug_enabled(facility)					\
          (((facility) & gupcr_debug_facility_mask) != 0)
#define gupcr_debug(facility, fmt, args...)				\
          if (gupcr_debug_enabled (facility))				\
            gupcr_debug_print("%s: " fmt "\n", __func__ , ##args)
#define gupcr_log(facility, fmt, args...)				\
          if (facility & gupcr_log_facility_mask)			\
            gupcr_log_print(fmt "\n", ##args)
#define gupcr_stats(facility, fmt, args...)				\
          if (facility & gupcr_stats_facility_mask)			\
            gupcr_stats_print(fmt "\n", ##args)
#define gupcr_trace(facility, fmt, args...)				\
          if (facility & gupcr_trace_facility_mask)			\
            gupcr_trace_print(fmt "\n", ##args)
#else
#define gupcr_debug_enabled(facility) (0)
#define gupcr_debug(facility, fmt, args...)
#define gupcr_log(facility, fmt, args...)
#define gupcr_stats(facility, fmt, args...)
#define gupcr_trace(facility, fmt, args...)
#endif

typedef enum
{
  FC_NONE      = 0b000000000000,
  FC_ADDR      = 0b000000000001,
  FC_ALLOC     = 0b000000000010,
  FC_ATOMIC    = 0b000000000100,
  FC_BARRIER   = 0b000000001000,
  FC_BROADCAST = 0b000000010000,
  FC_COLL      = 0b000000100000,
  FC_INFO      = 0b000001000000,
  FC_LOCK      = 0b000010000000,
  FC_MEM       = 0b000100000000,
  FC_MISC      = 0b001000000000,
  FC_PORTALS   = 0b010000000000,
  FC_SYSTEM    = 0b100000000000,
  FC_ALL       = 0b111111111111
} gupcr_facility_t;

#ifndef LONG_LONG_BITS
#define LONG_LONG_BITS (__SIZEOF_LONG_LONG__ * 8)
#endif /* LONG_LONG_BITS */

#ifndef SIZE_T_BITS
#define SIZE_T_BITS (__SIZEOF_SIZE_T__ * 8)
#endif /* SIZE_T_BITS */

/** Return TRUE, if 'v' is an exact power of 2.  */
static inline unsigned int
gupcr_is_pow_2 (unsigned long long v)
{
  return ((v & ~-v) == 0);
}

/** Return the value 'v' with the 1 bit at position 'bit' cleared.  */
static inline unsigned long long int
gupcr_clear_bit (unsigned long long v, unsigned int bit)
{
  return v & ~((unsigned long long) 1 << ((LONG_LONG_BITS - 1) - bit));
}

/** Return the value 'v' with the 1 bit at position 'bit' set.  */
static inline unsigned long long int
gupcr_set_bit (unsigned long long v, unsigned int bit)
{
  return v | ((unsigned long long) 1 << ((LONG_LONG_BITS - 1) - bit));
}

/** Return the position of the next non-zero bit in 'v'.
    If 'v' is zero, then the value LONG_LONG_BITS is returned.  */
static inline unsigned int
gupcr_find_first_one (unsigned long long v)
{
  return __builtin_clzll (v);
}

/** Return the largest power of 2 that is <= 'v'.  */
static inline unsigned int
gupcr_floor_log2 (unsigned long long v)
{
  return (LONG_LONG_BITS - 1) - __builtin_clzll (v);
}

/** Return the smallest power of 2 that is >= 'v'.  */
static inline unsigned int
gupcr_log2 (unsigned long long v)
{
  return gupcr_floor_log2 (v) + !gupcr_is_pow_2 (v);
}

#ifdef __UPC__

/* For ptrdiff_t definition */
#include <stddef.h>

/** Increment a shared pointer, by 'nbytes'.  */
static inline shared void *
gupcr_pts_add_offset (shared void *ptr, ptrdiff_t nbytes)
{
  return (shared void *) (((shared [] char *) ptr) + nbytes);
}

/** Return the difference between 'ptr1' and 'ptr2'. Both
    pointers must be non-NULL and have affinity to the same thread.  */
static inline ptrdiff_t
gupcr_pts_diff (shared void *ptr1, shared void *ptr2)
{
  return (ptrdiff_t) (((shared [] char *) ptr1) - ((shared [] char *) ptr2));
}

#endif /* __UPC__ */

extern gupcr_facility_t gupcr_debug_facility_mask;
extern gupcr_facility_t gupcr_log_facility_mask;
extern gupcr_facility_t gupcr_stats_facility_mask;
extern gupcr_facility_t gupcr_trace_facility_mask;

extern void gupcr_exit (void);
extern void gupcr_abort (void)
  __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern void gupcr_abort_with_msg (const char *fmt, ...)
  __attribute__ ((__format__ (__printf__, 1, 2)))
  __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern void gupcr_shutdown (int exit_code);
extern const char *gupcr_strerror (void);
extern const char *gupcr_get_pgm_name (void);
extern void gupcr_set_pgm_name (const char *pgm_name);
extern void gupcr_init_complete (void);
extern size_t gupcr_get_shared_heap_size (void);
extern void gupcr_signal_enable (int signal, void (*handler) (int));
extern void gupcr_signal_disable (int signal);
extern FILE *gupcr_fopen (const char *, const char *, const char *);
extern void gupcr_mkpath (const char *const);
extern long long gupcr_strtoll (const char *const,
				long long int, long long int, int *);
extern void gupcr_strtoll_error (const char *const str,
				 long long int, long long int val_max, int);
extern void gupcr_size_cvt_error (const char *const str, int);
extern const char *gupcr_get_buf_as_hex (char *bufstr,
					 const void *buf, size_t size);
extern const char *gupcr_get_pid_as_string (void);
extern size_t gupcr_get_shared_heap_size (void);
extern int gupcr_is_node_local_memory_enabled (void);
extern int gupcr_is_forcetouch_enabled (void);
extern void gupcr_unique_local_name (char *, const char *, int, int);
extern void gupcr_log_print (const char *fmt, ...)
  __attribute__ ((__format__ (__printf__, 1, 2)));
extern void gupcr_debug_print (const char *fmt, ...)
  __attribute__ ((__format__ (__printf__, 1, 2)));
extern void gupcr_error_print (const char *fmt, ...)
  __attribute__ ((__format__ (__printf__, 1, 2)));
extern void gupcr_info_print (const char *fmt, ...)
  __attribute__ ((__format__ (__printf__, 1, 2)));
extern void gupcr_trace_print (const char *fmt, ...)
  __attribute__ ((__format__ (__printf__, 1, 2)));
extern void gupcr_warn_print (const char *fmt, ...)
  __attribute__ ((__format__ (__printf__, 1, 2)));
extern void gupcr_utils_init (void);
extern void gupcr_utils_fini (void);

/* Called from: gupcr_env.c */
extern void gupcr_be_quiet (void);
extern void gupcr_no_warn (void);
extern void gupcr_set_shared_heap_size (size_t heap_size);
extern void gupcr_set_node_local_memory (int value);
extern void gupcr_set_forcetouch (int value);
extern void gupcr_set_debug_facility (gupcr_facility_t);
extern void gupcr_set_debug_filename (const char *);
extern void gupcr_set_log_facility (gupcr_facility_t);
extern void gupcr_set_log_filename (const char *);
extern void gupcr_set_stats_facility (gupcr_facility_t);
extern void gupcr_set_stats_filename (const char *);
extern void gupcr_set_trace_facility (gupcr_facility_t);
extern void gupcr_set_trace_filename (const char *);

/* See: gupcr_clock.c */
extern double gupcr_clock (void);
extern double gupcr_clock_resolution (void);
extern void gupcr_clock_init (void);

/* See: gupcr_env.c  */
extern void gupcr_env_init (void);

/* See: gupcr_pgm_info.c  */
extern void gupcr_validate_pgm_info (void);

//end lib_utils_api

/** @} */
#endif /* gupcr_utils.h */
