// A wrapper around malloc to enable a realloc implementation.

// Copyright (C) 2014-2016 Free Software Foundation, Inc.

// This file is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option) any
// later version.

// This file is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

        .version        3.1
        .target sm_30
        .address_size 64

.extern .func (.param.u64 %out_retval) malloc(.param.u64 %in_ar1);

// BEGIN GLOBAL FUNCTION DEF: __nvptx_malloc
.visible .func (.param.u64 %out_retval) __nvptx_malloc(.param.u64 %in_ar1)
{
        .reg.u64 %ar1;
.reg.u64 %retval;
        .reg.u64 %hr10;
        .reg.u64 %r26;
        .reg.u64 %r28;
        .reg.u64 %r29;
        .reg.u64 %r31;
        ld.param.u64 %ar1, [%in_ar1];
		mov.u64 %r26, %ar1;
		add.u64 %r28, %r26, 8;
        {
		.param.u64 %retval_in;
		.param.u64 %out_arg0;
		st.param.u64 [%out_arg0], %r28;
		call (%retval_in), malloc, (%out_arg0);
		ld.param.u64    %r29, [%retval_in];
        }
		st.u64  [%r29], %r26;
		add.u64 %r31, %r29, 8;
		mov.u64 %retval, %r31;
		st.param.u64    [%out_retval], %retval;
		ret;
}
