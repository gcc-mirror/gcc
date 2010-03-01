/* Declarations for upc-gasp.h
   Copyright (C) 2005-2009 Free Software Foundation, Inc.
   Gary Funck <gary@intrepid.com> and Nenad Vukicevic <nenad@intrepid.com>.
   Original Implementation by Adam Leko <leko@hcs.ufl.edu>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _UPC_GASP_H_
#define _UPC_GASP_H_

extern tree upc_gasp_add_src_args (tree, const char *, int);
extern tree upc_instrument_forall (location_t, int);
extern void upc_instrument_func (tree);

#endif /* !_UPC_GASP_H_ */
