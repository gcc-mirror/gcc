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

#ifndef _COLLECTORAPI_H
#define _COLLECTORAPI_H

/* This file contains function prototypes for the user-callable API
   routines in libcollector.  */

#include <pthread.h>

#ifdef __cplusplus
extern "C"
{
#endif
  /* Routine to record a sample in the experiment.  */
  extern void collector_sample (const char *name);

  /* Routine to suspend data collection during an experiment.  */
  extern void collector_pause (void);

  /* Routine to resume data collection during an experiment.  */
  extern void collector_resume (void);

  /* Routine to suspend per-thread data collection during an experiment.  */
  extern void collector_thread_pause (pthread_t tid);

  /* Routine to resume per-thread data collection during an experiment.  */
  extern void collector_thread_resume (pthread_t tid);

  /* Routine to close the experiment, and stop all data collection.  */
  extern void  collector_terminate_expt (void);

  typedef struct
  {
    unsigned int offset;
    unsigned int lineno;
  } Lineno;

  /* Routines to let libcollector know about dynamically loaded functions.  */
  extern void collector_func_load (const char *name, const char *alias,
				   const char *sourcename, void *vaddr,
				   int size, int lntsize, Lineno *lntable);

  extern void collector_func_unload (void *vaddr);

#ifdef NEED_COLLECTOR_MODULE
  extern void collector_module_load (const char *modulename, void *vaddr);
  extern void collector_module_unload (void *vaddr);
#endif

#ifdef __cplusplus
}
#endif

#endif /* _COLLECTORAPI_H */
