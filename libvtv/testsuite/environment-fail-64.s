	.file	"environment.cc"
	.section	.text._ZN15EnvironmentImpl6GetVarEPKcPc,"axG",@progbits,_ZN15EnvironmentImpl6GetVarEPKcPc,comdat
	.align 2
	.weak	_ZN15EnvironmentImpl6GetVarEPKcPc
	.type	_ZN15EnvironmentImpl6GetVarEPKcPc, @function
_ZN15EnvironmentImpl6GetVarEPKcPc:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	_ZTV11Environment@GOTPCREL(%rip), %rdx
	leaq	16(%rdx), %rdx
	movq	%rdx, (%rax)
	movl	$0, %eax
	testl	%eax, %eax
	je	.L3
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZdlPv@PLT
.L3:
	leave
	.cfi_def_cfa 7, 8
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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZN11EnvironmentD1Ev@PLT
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZdlPv@PLT
	leave
	.cfi_def_cfa 7, 8
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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	_ZTV11Environment@GOTPCREL(%rip), %rdx
	leaq	16(%rdx), %rdx
	movq	%rdx, (%rax)
	popq	%rbp
	.cfi_def_cfa 7, 8
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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZN11EnvironmentC2Ev@PLT
	movq	-8(%rbp), %rax
	movq	_ZTV15EnvironmentImpl@GOTPCREL(%rip), %rdx
	leaq	16(%rdx), %rdx
	movq	%rdx, (%rax)
	leave
	.cfi_def_cfa 7, 8
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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$8, %rsp
	.cfi_offset 3, -24
	movl	$8, %edi
	call	_Znwm@PLT
	movq	%rax, %rbx
	movq	$0, (%rbx)
	movq	%rbx, %rdi
	call	_ZN15EnvironmentImplC1Ev@PLT
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	$0, -8(%rbp)
	call	_ZN11Environment6CreateEv@PLT
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, %rsi
	leaq	_ZN4_VTVI11EnvironmentE12__vtable_mapE(%rip), %rdi
	call	_Z24__VLTVerifyVtablePointerPPvPKv@PLT
	addq	$16, %rax
	movq	(%rax), %rax
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rcx
	movl	$0, %esi
	movq	%rcx, %rdi
	call	*%rax
	movq	-16(%rbp), %rax
	movq	%rax, %rsi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE12:
	.size	main, .-main
	.weak	_ZTV11Environment
	.section	.data.rel.ro._ZTV11Environment,"awG",@progbits,_ZTV11Environment,comdat
	.align 32
	.type	_ZTV11Environment, @object
	.size	_ZTV11Environment, 40
_ZTV11Environment:
	.quad	0
	.quad	_ZTI11Environment
	.quad	_ZN11EnvironmentD1Ev
	.quad	_ZN11EnvironmentD0Ev
	.quad	__cxa_pure_virtual
	.weak	_ZTV15EnvironmentImpl
	.section	.data.rel.ro._ZTV15EnvironmentImpl,"awG",@progbits,_ZTV15EnvironmentImpl,comdat
	.align 32
	.type	_ZTV15EnvironmentImpl, @object
	.size	_ZTV15EnvironmentImpl, 40
_ZTV15EnvironmentImpl:
	.quad	0
	.quad	_ZTI15EnvironmentImpl
	.quad	_ZN15EnvironmentImplD1Ev
	.quad	_ZN15EnvironmentImplD0Ev
	.quad	_ZN15EnvironmentImpl6GetVarEPKcPc
	.section	.text._ZN15EnvironmentImplD2Ev,"axG",@progbits,_ZN15EnvironmentImplD5Ev,comdat
	.align 2
	.weak	_ZN15EnvironmentImplD2Ev
	.type	_ZN15EnvironmentImplD2Ev, @function
_ZN15EnvironmentImplD2Ev:
.LFB14:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA14
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	_ZTV15EnvironmentImpl@GOTPCREL(%rip), %rdx
	leaq	16(%rdx), %rdx
	movq	%rdx, (%rax)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
.LEHB0:
	call	_ZN11EnvironmentD2Ev@PLT
.LEHE0:
	movl	$0, %eax
	testl	%eax, %eax
	je	.L19
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZdlPv@PLT
	jmp	.L19
.L18:
	movq	%rax, %rdi
.LEHB1:
	call	_Unwind_Resume@PLT
.LEHE1:
.L19:
	leave
	.cfi_def_cfa 7, 8
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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZN15EnvironmentImplD1Ev@PLT
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZdlPv@PLT
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE16:
	.size	_ZN15EnvironmentImplD0Ev, .-_ZN15EnvironmentImplD0Ev
	.weak	_ZTS15EnvironmentImpl
	.section	.rodata._ZTS15EnvironmentImpl,"aG",@progbits,_ZTS15EnvironmentImpl,comdat
	.align 16
	.type	_ZTS15EnvironmentImpl, @object
	.size	_ZTS15EnvironmentImpl, 18
_ZTS15EnvironmentImpl:
	.string	"15EnvironmentImpl"
	.weak	_ZTI15EnvironmentImpl
	.section	.data.rel.ro._ZTI15EnvironmentImpl,"awG",@progbits,_ZTI15EnvironmentImpl,comdat
	.align 16
	.type	_ZTI15EnvironmentImpl, @object
	.size	_ZTI15EnvironmentImpl, 24
_ZTI15EnvironmentImpl:
	.quad	_ZTVN10__cxxabiv120__si_class_type_infoE+16
	.quad	_ZTS15EnvironmentImpl
	.quad	_ZTI11Environment
	.weak	_ZTI11Environment
	.section	.data.rel.ro._ZTI11Environment,"awG",@progbits,_ZTI11Environment,comdat
	.align 16
	.type	_ZTI11Environment, @object
	.size	_ZTI11Environment, 16
_ZTI11Environment:
	.quad	_ZTVN10__cxxabiv117__class_type_infoE+16
	.quad	_ZTS11Environment
	.weak	_ZTS11Environment
	.section	.rodata._ZTS11Environment,"aG",@progbits,_ZTS11Environment,comdat
	.type	_ZTS11Environment, @object
	.size	_ZTS11Environment, 14
_ZTS11Environment:
	.string	"11Environment"
	.hidden	_ZN4_VTVI11EnvironmentE12__vtable_mapE
	.weak	_ZN4_VTVI11EnvironmentE12__vtable_mapE
	.section	.vtable_map_vars,"awG",@progbits,_ZN4_VTVI11EnvironmentE12__vtable_mapE,comdat
	.align 8
	.type	_ZN4_VTVI11EnvironmentE12__vtable_mapE, @gnu_unique_object
	.size	_ZN4_VTVI11EnvironmentE12__vtable_mapE, 8
_ZN4_VTVI11EnvironmentE12__vtable_mapE:
	.zero	8
	.hidden	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE
	.weak	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE
	.section	.vtable_map_vars,"awG",@progbits,_ZN4_VTVI15EnvironmentImplE12__vtable_mapE,comdat
	.align 8
	.type	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE, @gnu_unique_object
	.size	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE, 8
_ZN4_VTVI15EnvironmentImplE12__vtable_mapE:
	.zero	8
	.section	.data.rel.ro,"aw",@progbits
	.align 16
	.type	__vptr_array_11Environment, @object
	.size	__vptr_array_11Environment, 16
__vptr_array_11Environment:
	.quad	_ZTV11Environment+16
	.quad	_ZTV15EnvironmentImpl+16
	.section	.rodata
	.align 8
.LC1:
	.string	"&"
	.string	""
	.string	""
	.ascii	"\224\tl\022_ZN4_VTVI11EnvironmentE12__vtable_mapE"
	.align 8
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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	leaq	__vptr_array_11Environment(%rip), %r8
	movl	$2, %ecx
	movl	$2, %edx
	leaq	.LC1(%rip), %rsi
	leaq	_ZN4_VTVI11EnvironmentE12__vtable_mapE(%rip), %rdi
	movq	_ZTV15EnvironmentImpl@GOTPCREL(%rip), %rax
	leaq	16(%rax), %rcx
	movl	$1, %edx
	leaq	.LC2(%rip), %rsi
	leaq	_ZN4_VTVI15EnvironmentImplE12__vtable_mapE(%rip), %rdi
	call	_Z17__VLTRegisterPairPPvPKvmS2_@PLT
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE17:
	.size	_GLOBAL__sub_I.00099_environment.cc, .-_GLOBAL__sub_I.00099_environment.cc
	.section	.init_array.00099,"aw"
	.align 8
	.quad	_GLOBAL__sub_I.00099_environment.cc
	.hidden	DW.ref.__gxx_personality_v0
	.weak	DW.ref.__gxx_personality_v0
	.section	.data.DW.ref.__gxx_personality_v0,"awG",@progbits,DW.ref.__gxx_personality_v0,comdat
	.align 8
	.type	DW.ref.__gxx_personality_v0, @object
	.size	DW.ref.__gxx_personality_v0, 8
DW.ref.__gxx_personality_v0:
	.quad	__gxx_personality_v0
	.ident	"GCC: (GNU) 4.9.0 20130616 (experimental)"
	.section	.note.GNU-stack,"",@progbits
