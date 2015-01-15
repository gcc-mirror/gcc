/* Copyright (C) 2014-2015 Free Software Foundation, Inc.

   Contributed by Mentor Embedded.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#define ABORT_PTX				\
  ".version 3.1\n"				\
  ".target sm_30\n"				\
  ".address_size 64\n"				\
  ".visible .func abort;\n"			\
  ".visible .func abort\n"			\
  "{\n"						\
  "trap;\n"					\
  "ret;\n"					\
  "}\n"						\
  ".visible .func _gfortran_abort;\n"		\
  ".visible .func _gfortran_abort\n"		\
  "{\n"						\
  "trap;\n"					\
  "ret;\n"					\
  "}\n" \

/* Generated with:

   $ echo 'int acc_on_device(int d) { return __builtin_acc_on_device(d); } int acc_on_device_h_(int *d) { return acc_on_device(*d); }' | accel-gcc/xgcc -Baccel-gcc -x c - -o - -S -m64 -O3 -fno-builtin-acc_on_device -fno-inline
*/
#define ACC_ON_DEVICE_PTX						\
  "        .version        3.1\n"					\
  "        .target sm_30\n"						\
  "        .address_size 64\n"						\
  ".visible .func (.param.u32 %out_retval)acc_on_device(.param.u32 %in_ar1);\n" \
  ".visible .func (.param.u32 %out_retval)acc_on_device(.param.u32 %in_ar1)\n" \
  "{\n"									\
  "        .reg.u32 %ar1;\n"						\
  ".reg.u32 %retval;\n"							\
  "        .reg.u64 %hr10;\n"						\
  "        .reg.u32 %r24;\n"						\
  "        .reg.u32 %r25;\n"						\
  "        .reg.pred %r27;\n"						\
  "        .reg.u32 %r30;\n"						\
  "        ld.param.u32 %ar1, [%in_ar1];\n"				\
  "                mov.u32 %r24, %ar1;\n"				\
  "                setp.ne.u32 %r27,%r24,4;\n"				\
  "                set.u32.eq.u32 %r30,%r24,5;\n"			\
  "                neg.s32 %r25, %r30;\n"				\
  "        @%r27   bra     $L3;\n"					\
  "                mov.u32 %r25, 1;\n"					\
  "$L3:\n"								\
  "                mov.u32 %retval, %r25;\n"				\
  "        st.param.u32    [%out_retval], %retval;\n"			\
  "        ret;\n"							\
  "        }\n"								\
  ".visible .func (.param.u32 %out_retval)acc_on_device_h_(.param.u64 %in_ar1);\n" \
  ".visible .func (.param.u32 %out_retval)acc_on_device_h_(.param.u64 %in_ar1)\n" \
  "{\n"									\
  "        .reg.u64 %ar1;\n"						\
  ".reg.u32 %retval;\n"							\
  "        .reg.u64 %hr10;\n"						\
  "        .reg.u64 %r25;\n"						\
  "        .reg.u32 %r26;\n"						\
  "        .reg.u32 %r27;\n"						\
  "        ld.param.u64 %ar1, [%in_ar1];\n"				\
  "                mov.u64 %r25, %ar1;\n"				\
  "                ld.u32  %r26, [%r25];\n"				\
  "        {\n"								\
  "                .param.u32 %retval_in;\n"				\
  "        {\n"								\
  "                .param.u32 %out_arg0;\n"				\
  "                st.param.u32 [%out_arg0], %r26;\n"			\
  "                call (%retval_in), acc_on_device, (%out_arg0);\n"	\
  "        }\n"								\
  "                ld.param.u32    %r27, [%retval_in];\n"		\
  "}\n"									\
  "                mov.u32 %retval, %r27;\n"				\
  "        st.param.u32    [%out_retval], %retval;\n"			\
  "        ret;\n"							\
  "        }"

 #define GOACC_INTERNAL_PTX						\
  ".version 3.1\n" \
  ".target sm_30\n" \
  ".address_size 64\n" \
  ".visible .func (.param .u32 %out_retval) GOACC_get_num_threads;\n" \
  ".visible .func (.param .u32 %out_retval) GOACC_get_thread_num;\n" \
  ".extern .func abort;\n" \
  ".visible .func (.param .u32 %out_retval) GOACC_get_num_threads\n"	\
  "{\n"									\
  ".reg .u32 %retval;\n"						\
  ".reg .u64 %hr10;\n"							\
  ".reg .u32 %r22;\n"							\
  ".reg .u32 %r23;\n"							\
  ".reg .u32 %r24;\n"							\
  ".reg .u32 %r25;\n"							\
  ".reg .u32 %r26;\n"							\
  ".reg .u32 %r27;\n"							\
  ".reg .u32 %r28;\n"							\
  ".reg .u32 %r29;\n"							\
  "mov.u32 %r26,0;\n"							\
  "{\n"									\
  ".param .u32 %retval_in;\n"						\
  "{\n"									\
  ".param .u32 %out_arg0;\n"						\
  "st.param.u32 [%out_arg0],%r26;\n"					\
  "call (%retval_in),GOACC_ntid,(%out_arg0);\n"				\
  "}\n"									\
  "ld.param.u32 %r27,[%retval_in];\n"					\
  "}\n"									\
  "mov.u32 %r22,%r27;\n"						\
  "mov.u32 %r28,0;\n"							\
  "{\n"									\
  ".param .u32 %retval_in;\n"						\
  "{\n"									\
  ".param .u32 %out_arg0;\n"						\
  "st.param.u32 [%out_arg0],%r28;\n"					\
  "call (%retval_in),GOACC_nctaid,(%out_arg0);\n"			\
  "}\n"									\
  "ld.param.u32 %r29,[%retval_in];\n"					\
  "}\n"									\
  "mov.u32 %r23,%r29;\n"						\
  "mul.lo.u32 %r24,%r22,%r23;\n"					\
  "mov.u32 %r25,%r24;\n"						\
  "mov.u32 %retval,%r25;\n"						\
  "st.param.u32 [%out_retval],%retval;\n"				\
  "ret;\n"								\
  "}\n"									\
  ".visible .func (.param .u32 %out_retval) GOACC_get_thread_num\n"	\
  "{\n"									\
  ".reg .u32 %retval;\n"						\
  ".reg .u64 %hr10;\n"							\
  ".reg .u32 %r22;\n"							\
  ".reg .u32 %r23;\n"							\
  ".reg .u32 %r24;\n"							\
  ".reg .u32 %r25;\n"							\
  ".reg .u32 %r26;\n"							\
  ".reg .u32 %r27;\n"							\
  ".reg .u32 %r28;\n"							\
  ".reg .u32 %r29;\n"							\
  ".reg .u32 %r30;\n"							\
  ".reg .u32 %r31;\n"							\
  ".reg .u32 %r32;\n"							\
  ".reg .u32 %r33;\n"							\
  "mov.u32 %r28,0;\n"							\
  "{\n"									\
  ".param .u32 %retval_in;\n"						\
  "{\n"									\
  ".param .u32 %out_arg0;\n"						\
  "st.param.u32 [%out_arg0],%r28;\n"					\
  "call (%retval_in),GOACC_ntid,(%out_arg0);\n"				\
  "}\n"									\
  "ld.param.u32 %r29,[%retval_in];\n"					\
  "}\n"									\
  "mov.u32 %r22,%r29;\n"						\
  "mov.u32 %r30,0;\n"							\
  "{\n"									\
  ".param .u32 %retval_in;\n"						\
  "{\n"									\
  ".param .u32 %out_arg0;\n"						\
  "st.param.u32 [%out_arg0],%r30;\n"					\
  "call (%retval_in),GOACC_ctaid,(%out_arg0);\n"			\
  "}\n"									\
  "ld.param.u32 %r31,[%retval_in];\n"					\
  "}\n"									\
  "mov.u32 %r23,%r31;\n"						\
  "mul.lo.u32 %r24,%r22,%r23;\n"					\
  "mov.u32 %r32,0;\n"							\
  "{\n"									\
  ".param .u32 %retval_in;\n"						\
  "{\n"									\
  ".param .u32 %out_arg0;\n"						\
  "st.param.u32 [%out_arg0],%r32;\n"					\
  "call (%retval_in),GOACC_tid,(%out_arg0);\n"				\
  "}\n"									\
  "ld.param.u32 %r33,[%retval_in];\n"					\
  "}\n"									\
  "mov.u32 %r25,%r33;\n"						\
  "add.u32 %r26,%r24,%r25;\n"						\
  "mov.u32 %r27,%r26;\n"						\
  "mov.u32 %retval,%r27;\n"						\
  "st.param.u32 [%out_retval],%retval;\n"				\
  "ret;\n"								\
  "}\n"
