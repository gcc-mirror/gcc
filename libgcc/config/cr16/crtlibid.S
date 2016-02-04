# Provide a weak definition of the library ID, for the benefit of certain
# configure scripts.	 
# Copyright (C) 2012-2016 Free Software Foundation, Inc.
# Contributed by KPIT Cummins Infosystems Limited.

# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.

	.ident  "GNU C crtlibid.o"

.weak __current_shared_library_r12_offset_
.set __current_shared_library_r12_offset_, 0
