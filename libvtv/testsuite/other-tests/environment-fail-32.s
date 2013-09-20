	.file	"environment.cc"
	.section	.text._ZN15EnvironmentImpl6GetVarEPKcPc,"axG",@progbits,_ZN15EnvironmentImpl6GetVarEPKcPc,comdat
	.align 2
	.weak	_ZN15EnvironmentImpl6GetVarEPKcPc
	.type	_ZN15EnvironmentImpl6GetVarEPKcPc, @function
_ZN15EnvironmentImpl6GetVarEPKcPc:
.LFB0:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	movl	$1, %eax
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE0:
	.size	_ZN15EnvironmentImpl6GetVarEPKcPc, .-_ZN15EnvironmentImpl6GetVarEPKcPc
	.text
	.align 2
	.globl	_ZN11EnvironmentD2Ev
	.type	_ZN11EnvironmentD2Ev, @function
_ZN11EnvironmentD2Ev:
.LFB2:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%ebx
	subl	$20, %esp
	.cfi_offset 3, -12
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	8(%ebp), %eax
	movl	_ZTV11Environment@GOT(%ebx), %edx
	leal	8(%edx), %edx
	movl	%edx, (%eax)
	movl	$0, %eax
	testl	%eax, %eax
	je	.L3
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	_ZdlPv@PLT
.L3:
	addl	$20, %esp
	popl	%ebx
	.cfi_restore 3
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE2:
	.size	_ZN11EnvironmentD2Ev, .-_ZN11EnvironmentD2Ev
	.globl	_ZN11EnvironmentD1Ev
	.set	_ZN11EnvironmentD1Ev,_ZN11EnvironmentD2Ev
	.align 2
	.globl	_ZN11EnvironmentD0Ev
	.type	_ZN11EnvironmentD0Ev, @function
_ZN11EnvironmentD0Ev:
.LFB4:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%ebx
	subl	$20, %esp
	.cfi_offset 3, -12
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	_ZN11EnvironmentD1Ev@PLT
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	_ZdlPv@PLT
	addl	$20, %esp
	popl	%ebx
	.cfi_restore 3
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE4:
	.size	_ZN11EnvironmentD0Ev, .-_ZN11EnvironmentD0Ev
	.section	.text._ZN11EnvironmentC2Ev,"axG",@progbits,_ZN11EnvironmentC5Ev,comdat
	.align 2
	.weak	_ZN11EnvironmentC2Ev
	.type	_ZN11EnvironmentC2Ev, @function
_ZN11EnvironmentC2Ev:
.LFB8:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	call	__x86.get_pc_thunk.cx
	addl	$_GLOBAL_OFFSET_TABLE_, %ecx
	movl	8(%ebp), %eax
	movl	_ZTV11Environment@GOT(%ecx), %edx
	leal	8(%edx), %edx
	movl	%edx, (%eax)
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE8:
	.size	_ZN11EnvironmentC2Ev, .-_ZN11EnvironmentC2Ev
	.weak	_ZN11EnvironmentC1Ev
	.set	_ZN11EnvironmentC1Ev,_ZN11EnvironmentC2Ev
	.section	.text._ZN15EnvironmentImplC2Ev,"axG",@progbits,_ZN15EnvironmentImplC5Ev,comdat
	.align 2
	.weak	_ZN15EnvironmentImplC2Ev
	.type	_ZN15EnvironmentImplC2Ev, @function
_ZN15EnvironmentImplC2Ev:
.LFB10:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%ebx
	subl	$20, %esp
	.cfi_offset 3, -12
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	_ZN11EnvironmentC2Ev@PLT
	movl	8(%ebp), %eax
	movl	_ZTV15EnvironmentImpl@GOT(%ebx), %edx
	leal	8(%edx), %edx
	movl	%edx, (%eax)
	addl	$20, %esp
	popl	%ebx
	.cfi_restore 3
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE10:
	.size	_ZN15EnvironmentImplC2Ev, .-_ZN15EnvironmentImplC2Ev
	.weak	_ZN15EnvironmentImplC1Ev
	.set	_ZN15EnvironmentImplC1Ev,_ZN15EnvironmentImplC2Ev
	.text
	.align 2
	.globl	_ZN11Environment6CreateEv
	.type	_ZN11Environment6CreateEv, @function
_ZN11Environment6CreateEv:
.LFB5:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%esi
	pushl	%ebx
	subl	$16, %esp
	.cfi_offset 6, -12
	.cfi_offset 3, -16
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	$4, (%esp)
	call	_Znwj@PLT
	movl	%eax, %esi
	movl	$0, (%esi)
	movl	%esi, (%esp)
	call	_ZN15EnvironmentImplC1Ev@PLT
	movl	%esi, %eax
	addl	$16, %esp
	popl	%ebx
	.cfi_restore 3
	popl	%esi
	.cfi_restore 6
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE5:
	.size	_ZN11Environment6CreateEv, .-_ZN11Environment6CreateEv
	.section	.rodata
.LC0:
	.string	"%p\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB12:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%ebx
	andl	$-16, %esp
	subl	$32, %esp
	.cfi_offset 3, -12
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	$0, 28(%esp)
	call	_ZN11Environment6CreateEv@PLT
	movl	%eax, 24(%esp)
	movl	24(%esp), %eax
	movl	(%eax), %eax
	movl	%eax, 4(%esp)
	leal	_ZN4_VTVI11EnvironmentE12__vtable_mapE@GOTOFF(%ebx), %eax
	movl	%eax, (%esp)
	call	_Z24__VLTVerifyVtablePointerPPvPKv@PLT
	addl	$8, %eax
	movl	(%eax), %eax
	movl	28(%esp), %edx
	movl	%edx, 8(%esp)
	movl	$0, 4(%esp)
	movl	24(%esp), %edx
	movl	%edx, (%esp)
	call	*%eax
	movl	24(%esp), %eax
	movl	%eax, 4(%esp)
	leal	.LC0@GOTOFF(%ebx), %eax
	movl	%eax, (%esp)
	call	printf@PLT
	movl	$0, %eax
	movl	-4(%ebp), %ebx
	leave
	.cfi_restore 5
	.cfi_restore 3
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE12:
	.size	main, .-main
	.weak	_ZTV11Environment
	.section	.data.rel.ro._ZTV11Environment,"awG",@progbits,_ZTV11Environment,comdat
	.align 8
	.type	_ZTV11Environment, @object
	.size	_ZTV11Environment, 20
_ZTV11Environment:
	.long	0
	.long	_ZTI11Environment
	.long	_ZN11EnvironmentD1Ev
	.long	_ZN11EnvironmentD0Ev
	.long	__cxa_pure_virtual
	.weak	_ZTV15EnvironmentImpl
	.section	.data.rel.ro._ZTV15EnvironmentImpl,"awG",@progbits,_ZTV15EnvironmentImpl,comdat
	.align 8
	.type	_ZTV15EnvironmentImpl, @object
	.size	_ZTV15EnvironmentImpl, 20
_ZTV15EnvironmentImpl:
	.long	0
	.long	_ZTI15EnvironmentImpl
	.long	_ZN15EnvironmentImplD1Ev
	.long	_ZN15EnvironmentImplD0Ev
	.long	_ZN15EnvironmentImpl6GetVarEPKcPc
	.section	.text._ZN15EnvironmentImplD2Ev,"axG",@progbits,_ZN15EnvironmentImplD5Ev,comdat
	.align 2
	.weak	_ZN15EnvironmentImplD2Ev
	.type	_ZN15EnvironmentImplD2Ev, @function
_ZN15EnvironmentImplD2Ev:
.LFB14:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA14
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%ebx
	subl	$20, %esp
	.cfi_offset 3, -12
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	8(%ebp), %eax
	movl	_ZTV15EnvironmentImpl@GOT(%ebx), %edx
	leal	8(%edx), %edx
	movl	%edx, (%eax)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
.LEHB0:
	call	_ZN11EnvironmentD2Ev@PLT
.LEHE0:
	movl	$0, %eax
	testl	%eax, %eax
	je	.L19
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	_ZdlPv@PLT
	jmp	.L19
.L18:
	movl	%eax, (%esp)
.LEHB1:
	call	_Unwind_Resume@PLT
.LEHE1:
.L19:
	addl	$20, %esp
	popl	%ebx
	.cfi_restore 3
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE14:
	.globl	__gxx_personality_v0
	.section	.gcc_except_table._ZN15EnvironmentImplD2Ev,"aG",@progbits,_ZN15EnvironmentImplD5Ev,comdat
.LLSDA14:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE14-.LLSDACSB14
.LLSDACSB14:
	.uleb128 .LEHB0-.LFB14
	.uleb128 .LEHE0-.LEHB0
	.uleb128 .L18-.LFB14
	.uleb128 0
	.uleb128 .LEHB1-.LFB14
	.uleb128 .LEHE1-.LEHB1
	.uleb128 0
	.uleb128 0
.LLSDACSE14:
	.section	.text._ZN15EnvironmentImplD2Ev,"axG",@progbits,_ZN15EnvironmentImplD5Ev,comdat
	.size	_ZN15EnvironmentImplD2Ev, .-_ZN15EnvironmentImplD2Ev
	.weak	_ZN15EnvironmentImplD1Ev
	.set	_ZN15EnvironmentImplD1Ev,_ZN15EnvironmentImplD2Ev
	.section	.text._ZN15EnvironmentImplD0Ev,"axG",@progbits,_ZN15EnvironmentImplD0Ev,comdat
	.align 2
	.weak	_ZN15EnvironmentImplD0Ev
	.type	_ZN15EnvironmentImplD0Ev, @function
_ZN15EnvironmentImplD0Ev:
.LFB16:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%ebx
	subl	$20, %esp
	.cfi_offset 3, -12
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	_ZN15EnvironmentImplD1Ev@PLT
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	_ZdlPv@PLT
	addl	$20, %esp
	popl	%ebx
	.cfi_restore 3
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE16:
	.size	_ZN15EnvironmentImplD0Ev, .-_ZN15EnvironmentImplD0Ev
	.weak	_ZTS15EnvironmentImpl
	.section	.rodata._ZTS15EnvironmentImpl,"aG",@progbits,_ZTS15EnvironmentImpl,comdat
	.type	_ZTS15EnvironmentImpl, @object
	.size	_ZTS15EnvironmentImpl, 18
_ZTS15EnvironmentImpl:
	.string	"15EnvironmentImpl"
	.weak	_ZTI15EnvironmentImpl
	.section	.data.rel.ro._ZTI15EnvironmentImpl,"awG",@progbits,_ZTI15EnvironmentImpl,comdat
	.align 4
	.type	_ZTI15EnvironmentImpl, @object
	.size	_ZTI15EnvironmentImpl, 12
_ZTI15EnvironmentImpl:
	.long	_ZTVN10__cxxabiv120__si_class_type_infoE+8
	.long	_ZTS15EnvironmentImpl
	.long	_ZTI11Environment
	.weak	_ZTI11Environment
	.section	.data.rel.ro._ZTI11Environment,"awG",@progbits,_ZTI11Environment,comdat
	.align 4
	.type	_ZTI11Environment, @object
	.size	_ZTI11Environment, 8
_ZTI11Environment:
	.long	_ZTVN10__cxxabiv117__class_type_infoE+8
	.long	_ZTS11Environment
	.weak	_ZTS11Environment
	.section	.rodata._ZTS11Environment,"aG",@progbits,_ZTS11Environment,comdat
	.type	_ZTS11Environment, @object
	.size	_ZTS11Environment, 14
_ZTS11Environment:
	.string	"11Environment"
	.hidden	_ZN4_VTVI11EnvironmentE12__vtable_mapE
	.weak	_ZN4_VTVI11EnvironmentE12__vtable_mapE
	.section	.vtable_map_vars,"awG",@progbits,_ZN4_VTVI11EnvironmentE12__vtable_mapE,comdat
	.align 4
	.type	_ZN4_VTVI11EnvironmentE12__vtable_mapE, @gnu_unique_object
	.size	_ZN4_VTVI11EnvironmentE12__vtable_mapE, 4
_ZN4_VTVI11EnvironmentE12__vtable_mapE:
	.zero	4
	.hidden	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE
	.weak	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE
	.section	.vtable_map_vars,"awG",@progbits,_ZN4_VTVI15EnvironmentImplE12__vtable_mapE,comdat
	.align 4
	.type	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE, @gnu_unique_object
	.size	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE, 4
_ZN4_VTVI15EnvironmentImplE12__vtable_mapE:
	.zero	4
	.section	.data.rel.ro,"aw",@progbits
	.align 4
	.type	__vptr_array_11Environment, @object
	.size	__vptr_array_11Environment, 8
__vptr_array_11Environment:
	.long	_ZTV11Environment+8
	.long	_ZTV15EnvironmentImpl+8
	.section	.rodata
	.align 4
.LC1:
	.string	"&"
	.string	""
	.string	""
	.ascii	"\224\tl\022_ZN4_VTVI11EnvironmentE12__vtable_mapE"
	.align 4
.LC2:
	.string	"*"
	.string	""
	.string	""
	.ascii	"N\225\r\334_ZN4_VTVI15EnvironmentImplE12__vtable_mapE"
	.text
	.type	_GLOBAL__sub_I.00099_environment.cc, @function
_GLOBAL__sub_I.00099_environment.cc:
.LFB17:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%ebx
	subl	$36, %esp
	.cfi_offset 3, -12
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	leal	__vptr_array_11Environment@GOTOFF(%ebx), %eax
	movl	%eax, 16(%esp)
	movl	$2, 12(%esp)
	movl	$2, 8(%esp)
	leal	.LC1@GOTOFF(%ebx), %eax
	movl	%eax, 4(%esp)
	leal	_ZN4_VTVI11EnvironmentE12__vtable_mapE@GOTOFF(%ebx), %eax
	movl	%eax, (%esp)
	movl	_ZTV15EnvironmentImpl@GOT(%ebx), %eax
	leal	8(%eax), %eax
	movl	%eax, 12(%esp)
	movl	$1, 8(%esp)
	leal	.LC2@GOTOFF(%ebx), %eax
	movl	%eax, 4(%esp)
	leal	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE@GOTOFF(%ebx), %eax
	movl	%eax, (%esp)
	call	_Z17__VLTRegisterPairPPvPKvjS2_@PLT
	addl	$36, %esp
	popl	%ebx
	.cfi_restore 3
	popl	%ebp
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE17:
	.size	_GLOBAL__sub_I.00099_environment.cc, .-_GLOBAL__sub_I.00099_environment.cc
	.section	.init_array.00099,"aw"
	.align 4
	.long	_GLOBAL__sub_I.00099_environment.cc
	.section	.text.__x86.get_pc_thunk.cx,"axG",@progbits,__x86.get_pc_thunk.cx,comdat
	.globl	__x86.get_pc_thunk.cx
	.hidden	__x86.get_pc_thunk.cx
	.type	__x86.get_pc_thunk.cx, @function
__x86.get_pc_thunk.cx:
.LFB18:
	.cfi_startproc
	movl	(%esp), %ecx
	ret
	.cfi_endproc
.LFE18:
	.section	.text.__x86.get_pc_thunk.bx,"axG",@progbits,__x86.get_pc_thunk.bx,comdat
	.globl	__x86.get_pc_thunk.bx
	.hidden	__x86.get_pc_thunk.bx
	.type	__x86.get_pc_thunk.bx, @function
__x86.get_pc_thunk.bx:
.LFB19:
	.cfi_startproc
	movl	(%esp), %ebx
	ret
	.cfi_endproc
.LFE19:
	.hidden	DW.ref.__gxx_personality_v0
	.weak	DW.ref.__gxx_personality_v0
	.section	.data.DW.ref.__gxx_personality_v0,"awG",@progbits,DW.ref.__gxx_personality_v0,comdat
	.align 4
	.type	DW.ref.__gxx_personality_v0, @object
	.size	DW.ref.__gxx_personality_v0, 4
DW.ref.__gxx_personality_v0:
	.long	__gxx_personality_v0
	.ident	"GCC: (GNU) 4.9.0 20130616 (experimental)"
	.section	.note.GNU-stack,"",@progbits
