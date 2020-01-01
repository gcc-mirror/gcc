// Exception handling and frame unwind runtime interface routines.
// Copyright (C) 2011-2020 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

module gcc.unwind;

import gcc.config;

static if (GNU_ARM_EABI_Unwinder)
{
    version (ARM)
        public import gcc.unwind.arm;
    else version (TIC6X)
        public import gcc.unwind.c6x;
    else
        static assert(false, "Unsupported target for ARM_EABI_UNWINDER");
}
else
    public import gcc.unwind.generic;
