# classes.pl - A perl program to generate most of the contents of
# javaprims.h automatically.

# Copyright (C) 1998, 1999, 2000, 2002, 2005  Free Software Foundation
#
# This file is part of libgcj.
#
# This software is copyrighted work licensed under the terms of the
# Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
# details.

# Usage: cd <build>/classpath/lib ; perl classes.pl.

use DirHandle;

if (-d 'java')
{
    # Ok here.
}
elsif (-d '../java')
{
    chdir ('..');
}
else
{
    die "couldn't find java directory\n";
}

&scan ('java', 2);

exit 0;

sub scan
{
    local ($dir, $indent) = @_;
    local (@subdirs) = ();
    local (%classes) = ();

    local ($d) = new DirHandle $dir;
    local (*JFILE);
    local ($name);
    if (defined $d)
    {
	while (defined ($name = $d->read))
	{
	    next if $name eq 'CVS';
	    next if $name eq '.';
	    next if $name eq '..';
	    if ($dir eq 'java'
		&& $name ne 'lang'
		&& $name ne 'util'
		&& $name ne 'io')
	    {
		# We only generate decls for java.lang, java.io, and
		# java.util.
		next;
	    }
	    if (-d ($dir . '/' . $name))
	    {
		push (@subdirs, $name);
		next;
	    }
	    next unless $name =~ s/\.class$//;
	    $classes{$name} = 1;
	}

	undef $d;
    }

    local ($spaces) = ' ' x $indent;
    local ($classname);
    ($classname = $dir) =~ s/^.*\///;
    print $spaces, "namespace ", $classname, "\n";
    print $spaces, "{\n";

    foreach (sort keys %classes)
    {
	print $spaces, "  class ", $_, ";\n";
    }
    print "\n" if scalar @classes > 0 && scalar @subdirs > 0;

    local ($first) = 1;
    foreach (sort @subdirs)
    {
	print "\n" unless $first;
	$first = 0;
	&scan ("$dir/$_", $indent + 2);
    }

    print $spaces, "}\n";
}
