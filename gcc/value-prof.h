/* Definitions for transformations based on profile information for values.
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Supported histogram types.  */
enum hist_type
{
  HIST_TYPE_INTERVAL,	/* Measures histogram of values inside a specified
			   interval.  */
  HIST_TYPE_POW2,	/* Histogram of power of 2 values.  */
  HIST_TYPE_SINGLE_VALUE, /* Tries to identify the value that is (almost)
			   always constant.  */
  HIST_TYPE_CONST_DELTA	/* Tries to identify the (almost) always constant
			   difference between two evaluations of a value.  */
};

#define COUNTER_FOR_HIST_TYPE(TYPE) ((int) (TYPE) + GCOV_FIRST_VALUE_COUNTER)
#define HIST_TYPE_FOR_COUNTER(COUNTER) \
  ((enum hist_type) ((COUNTER) - GCOV_FIRST_VALUE_COUNTER))

/* The value to measure.  */
struct histogram_value
{
  rtx value;		/* The value to profile.  */
  enum machine_mode mode; /* And its mode.  */
  rtx seq;		/* Insns required to count the profiled value.  */
  rtx insn;		/* Insn before that to measure.  */
  enum hist_type type;	/* Type of information to measure.  */
  unsigned n_counters;	/* Number of required counters.  */
  union
    {
      struct
	{
	  int int_start;	/* First value in interval.  */
	  int steps;		/* Number of values in it.  */
	  int may_be_less;	/* May the value be below?  */
	  int may_be_more;	/* Or above.  */
	} intvl;	/* Interval histogram data.  */
      struct
	{
	  int may_be_other;	/* If the value may be non-positive or not 2^k.  */
	} pow2;		/* Power of 2 histogram data.  */
    } hdata;		/* Profiled information specific data.  */
};

extern void find_values_to_profile (unsigned *, struct histogram_value **);
extern void free_profiled_values (unsigned, struct histogram_value *);
extern bool value_profile_transformations (void);
