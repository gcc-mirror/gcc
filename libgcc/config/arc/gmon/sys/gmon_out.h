/* Copyright (C) 2007-2016 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define GMON_TAG_TIME_HIST 0
#define GMON_TAG_CG_ARC 1
#define GMON_TAG_BB_COUNT 2

#define GMON_MAGIC "gmon"
#define GMON_VERSION 1

struct arc_gmon_hist_hdr
{
  char low_pc[4];
  char high_pc[4];
  char hist_size[4];
  char prof_rate[4];
  char dimen[15];
  char dimen_abbrev;
};

struct gmon_cg_arc_record
{
  char afrompc[4];
  char selfpc[4];
  char count[4];
};

struct gmon_hdr
{
  char cookie[4];
  char version[4];
  char c[12];
};
