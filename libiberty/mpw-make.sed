# Sed commands to finish translating libiberty's Unix makefile to MPW syntax.

# Comment out a useless thing.
/^\.always\./s/^/#/

# Replace the auto-generated list with the list of what we know we need.
s/`cat needed-list`/"{o}"alloca.c.o "{o}"bcopy.c.o "{o}"getpagesize.c.o "{o}"insque.c.o "{o}"mpw.c.o "{o}"strcasecmp.c.o "{o}"strdup.c.o "{o}"strncasecmp.c.o/

# Paste in some desirable definitions.
# The default rule here completely replaces the tricky stuff in the Unix
# Makefile.in.
/^###$/a\
\
HDEFINES = -d NEED_sys_siglist -d NEED_sys_errlist -d NEED_basename -d NEED_strcasecmp -d NEED_strncasecmp\
INCLUDES = -i : -i {INCDIR}: -i {INCDIR}:mpw: -i ::extra-include: -i "{s}"\
\
.c.o \\Option-f .c\
	{CC} @DASH_C_FLAG@ {DepDir}{Default}.c {LIBCFLAGS} {INCLUDES} {HDEFINES} @SEGMENT_FLAG({Default})@ -o {TargDir}{Default}.c.o\

# Remove dependency on needed-list, which we don't use.
/DO_ALSO =/s/needed-list//

/INCDIR=/s/"{srcdir}"{MULTISRCTOP}::/"{topsrcdir}"/

# Whack out the COMPILE.c trickiness.
/^COMPILE.c /,/^$/d

# Remove the multido trickiness from the "all" target.
/^all \\Option-f/,/^$/c\
all \\Option-f {TARGETLIB}\


# Remove the RULE1/RULE2 crud.
/if \[/,/fi/d
/^RULE1 =/,/RULE2 =/d
/RULE2/s/RULE2/TARGETLIB/

# Don't want fdmatch ever.
s/ "{o}"fdmatch.c.o//

# Fix paths to generated files.
/config.h/s/"{s}"config.h/"{o}"config.h/

# Whack out config rebuild rules.
/^"{o}"config.h \\Option-f/,/^$/d






