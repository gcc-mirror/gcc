// A wrapper around free to enable a realloc implementation.

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

.extern .func free(.param.u64 %in_ar1);

// BEGIN GLOBAL FUNCTION DEF: __nvptx_free
.visible .func __nvptx_free(.param.u64 %in_ar1)
{
	.reg.u64 %ar1;
	.reg.u64 %hr10;
	.reg.u64 %r23;
	.reg.pred %r25;
	.reg.u64 %r27;
	ld.param.u64 %ar1, [%in_ar1];
		mov.u64	%r23, %ar1;
		setp.eq.u64 %r25,%r23,0;
	@%r25	bra	$L1;
		add.u64	%r27, %r23, -8;
	{
		.param.u64 %out_arg0;
		st.param.u64 [%out_arg0], %r27;
		call free, (%out_arg0);
	}
$L1:
	ret;
	}
