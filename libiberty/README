This directory contains the -liberty library of free software.
It is a collection of subroutines used by various GNU programs.
Current members include:

	getopt -- get options from command line
	obstack -- stacks of arbitrarily-sized objects
	strerror -- error message strings corresponding to errno
	strtol -- string-to-long conversion
	strtoul -- string-to-unsigned-long conversion

We expect many of the GNU subroutines that are floating around to
eventually arrive here.

To build the library, do:

	./configure HOSTTYPE
	make

Please report bugs and fixes to "bug-gnu-utils@prep.ai.mit.edu".  Thank you.

ADDING A NEW FILE
=================

There are two sets of files:  Those that are "required" will be
included in the library for all configurations, while those
that are "optional" will be included in the library only if "needed."

To add a new required file, edit Makefile to add the source file
name to CFILES and the object file to REQUIRED_OFILES.

Adding a new optional file is more fragile.  As a general rule,
an optional file will be included in the library if it provides
functionality missing in the "standard" C library.
For most hosts, the Makefile automatically figures out which
functionality is missing by compiling and linking a dummy test
program, and examining the error messages.

So to get this to work, you should do the following:

1) Select one function defined in the file you're adding.
For example, the getcwd function.
2) Add that function to the list in the file functions.def.
3) The name of the new file must be the same as the function
you've chosen with the .c suffix added.  E.g. getcwd() must be
defined in getcwd.c.  (The file can define other functions as well.)
4) In Makefile.in, add the name of the source file (e.g. getcwd.c)
to CFILES.

The file you've added (e.g. getcwd.c) should compile and work
on all hosts where it is needed (e.g. not found when linking
the dummy.c program).  It does not have to work or even
compile on hosts where it is not needed.

HOW THE AUTOMATIC CONFIGURATION WORKS
=====================================

The libiberty.a target (in RULE1) depends on $(DO_ALSO).
For normal configurations, DO_ALSO=needed-list.

So needed-list is first made.  The needed-list rule compiles
dummy.c.  Because dummy.c includes functions.def, the
resulting object file will contain a call to each of the
optional functions (for simplicity assume each optional file
defines a single function).  This object file will be linked
against the standard libraries (as defined by using $(CC)
and various flags).  Any function missing will causes the
linker to emit an error message.  We assume the name
of the missing function(s) are in the error message(s).
The awk script find-needed.awk has been generated from
functions.def.  It is used to search the linker output
messages for words that match the functions listed in
functions.def.  The list of functions found is written
on a single line to the file needed-list.

After needed-list has been generated, the libiberty.a
target (in RULE1) just calls 'make' recursively.
It passes the contents of needed-list using the
definition (expanded) HOST_OFILES="`cat needed-list`".
It also tells the inferior 'make' to use RULE2.

The inferior 'make' is very conventional:  The main
rule is $(RULE2) (which is libiberty.a).  It depends
on a list of object files: $(REQUIRED_OFILES) $(HOST_OFILES)
(and $(EXTRA_OFILES), which is usually empty).  The superior
'make' passes in $(HOST_OFILES); the others are fixed
in the Makefile.

ADDING A NEW CONFIGURATION
==========================

On most hosts you should be able to use the scheme for automatically
figuring out which files are needed.  In that case, you probably
don't need a special Makefile stub for that configuration.

If the fully automatic scheme doesn't work, you may be able to get
by with defining EXTRA_OFILES in your Makefile stub.  This is
a list of object file names that should be treated as required
for this configuration - they will be included in libiberty.a,
regardless of whatever might be in the C library.  Moreover,
when the dummy.c program is linked, it will be linked with
$(EXTRA_OFILES).  Therefore, if a function in functions.def
is defined by one of the EXTRA_OFILES, it will not be listed as
"needed".  Thus if your hal9000 host needs a special implementation
of getcwd, you can just create hal9000-getcwd.c, and define:
	EXTRA_OFILES=hal9000-getcwd.o
Or if you want to use the libiberty version of strstr(),
even though there is a version in the C library (it might be
buggy or slow), just define:
	EXTRA_OFILES=strstr.o

You can create a "manual" host configuration FOO with a file
config/mh-FOO.  In it, the HOST_OFILES macro should explicitly
list that subset of the optional files that should be in the
library.  You should also set:
	DO_ALSO =
This overrides all of the magic needed to automatically
determine which files are "needed."  However, keeping that list
up to date is another matter...

HOW THE MANUAL CONFIGURATION WORKS
==================================

This also uses a recursive make, but the superior make
does not do anything interesting - it just calls the
inferior make with HOST_OFILES defined as $(HOST_OFILES),
which is the list you created in your configuration.

You probably don't want to depend on manual configuration,
because keeping the HOST_OFILES list up-to-date will be a pain.
