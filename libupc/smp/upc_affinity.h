/* Copyright (C) 2008 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#ifndef _UPC_AFFINITY_H_
#define _UPC_AFFINITY_H_

extern void __upc_affinity_cpu_avoid_free (const upc_cpu_avoid_p);
extern upc_cpu_avoid_p __upc_affinity_cpu_avoid_new (void);
extern void __upc_affinity_cpu_avoid_set (const int, const upc_cpu_avoid_p);
extern int __upc_affinity_init (const upc_info_p, const upc_cpu_avoid_p,
				const char **err_msg);
extern void __upc_affinity_set (const upc_info_p, const int);
extern int __upc_affinity_supported (void);

#endif /* !_UPC_AFFINITY_H_ */
