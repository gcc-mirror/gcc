	.version 3.1
	.target	sm_30
	.address_size 64

.global .u64 %__exitval;
// BEGIN GLOBAL FUNCTION DEF: abort
.visible .func abort
{
        .reg .u64 %rd1;
        ld.global.u64   %rd1,[%__exitval];
        st.u32   [%rd1], 255;
        exit;
}
// BEGIN GLOBAL FUNCTION DEF: exit
.visible .func exit (.param .u32 %arg)
{
        .reg .u64 %rd1;
	.reg .u32 %val;
	ld.param.u32 %val,[%arg];
        ld.global.u64   %rd1,[%__exitval];
        st.u32   [%rd1], %val;
        exit;
}

.extern .func (.param.u32 retval) main (.param.u32 argc, .param.u64 argv);

.visible .entry __main (.param .u64 __retval, .param.u32 __argc, .param.u64 __argv)
{
        .reg .u32 %r<3>;
        .reg .u64 %rd<3>;
	.param.u32 %argc;
	.param.u64 %argp;
	.param.u32 %mainret;
        ld.param.u64    %rd0, [__retval];
        st.global.u64   [%__exitval], %rd0;

	ld.param.u32	%r1, [__argc];
	ld.param.u64	%rd1, [__argv];
	st.param.u32	[%argc], %r1;
	st.param.u64	[%argp], %rd1;
        call.uni        (%mainret), main, (%argc, %argp);
	ld.param.u32	%r1,[%mainret];
        st.s32   [%rd0], %r1;
        exit;
}
