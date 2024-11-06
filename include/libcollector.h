/* Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by Oracle.

   This file is part of GNU Binutils.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#ifndef _LIBCOLLECTOR_H
#define _LIBCOLLECTOR_H

typedef struct
{
  unsigned int offset;
  unsigned int lineno;
} Lineno;

#ifdef __cplusplus
extern "C"
{
#endif

  /* This file contains function prototypes for the user-callable API
     routines in libcollector for C and C++ codes.  */

  /* Routine to record a sample in the experiment.  */
  void collector_sample (char *name);

  /* Routine to suspend data collection during an experiment.  */
  void collector_pause (void);

  /* Routine to resume data collection during an experiment.  */
  void collector_resume (void);

  /* Routine to suspend per-thread data collection during an experiment.  */
  void collector_thread_pause (unsigned int tid);

  /* Routine to resume per-thread data collection during an experiment.  */
  void collector_thread_resume (unsigned int tid);

  /* Routine to close the experiment, and stop all data collection.  */
  void collector_terminate_expt (void);

  /* Routines to let libcollector know about a dynamically loaded function.  */
  void collector_func_load (char *name, char *alias, char *sourcename,
			  void *vaddr, int size, int lntsize, Lineno *lntable);
  void collector_func_unload (void *vaddr);

  /* Define the weak symbols for the API.  */
  void collector_sample () __attribute__ ((weak));
  void collector_pause () __attribute__ ((weak));
  void collector_resume () __attribute__ ((weak));
  void collector_thread_pause () __attribute__ ((weak));
  void collector_thread_resume () __attribute__ ((weak));
  void collector_terminate_expt () __attribute__ ((weak));
  void collector_func_load () __attribute__ ((weak));
  void collector_func_unload () __attribute__ ((weak));

#ifdef __cplusplus
}
#endif

/* Define the macros that actually get inserted in the caller's code.  */
#define collector_sample(x)	(collector_sample ? collector_sample(x), 0 : 0)
#define collector_pause()	(collector_pause ? collector_pause(), 0 : 0)
#define collector_resume()	(collector_resume ? collector_resume(),0 : 0
#define collector_thread_pause(tid) \
	(collector_thread_pause ? collector_thread_pause(tid), 0 : 0)
#define collector_thread_resume(tid) \
	(collector_thread_resume ? collector_thread_resume(tid), 0 : 0)
#define collector_terminate_expt() \
	(collector_terminate_expt ? collector_terminate_expt(), 0 : 0)
#define collector_func_load(x0,x1,x2,x3,x4,x5,x6) \
	collector_func_load ? collector_func_load(x0,x1,x2,x3,x4,x5,x6), 0 : 0)
#define collector_func_unload(x) \
	(collector_func_unload ? collector_func_unload(x), 0 : 0)
#endif /* _LIBCOLLECTOR_H */
