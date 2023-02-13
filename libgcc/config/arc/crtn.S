/* Ensure .fini/.init return for the Synopsys DesignWare ARC CPU.

   Copyright (C) 1994-2023 Free Software Foundation, Inc.
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

# This file just makes sure that the .fini and .init sections do in
# fact return. This file is the last thing linked into any executable.

#ifdef __ARC_RF16__
	/* Use object attributes to inform other tools this file is
	safe for RF16 configuration.  */
	.arc_attribute Tag_ARC_ABI_rf16, 1
#endif
	.section .init
	pop_s	blink
	j_s	[blink]


	.section .fini
	pop_s	blink
	j_s	[blink]
