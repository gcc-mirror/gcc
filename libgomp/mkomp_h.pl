#!/usr/bin/perl -w
#  Copyright (C) 2005 Free Software Foundation, Inc.
#  Contributed by Richard Henderson <rth@redhat.com>.
#
#  This file is part of the GNU OpenMP Library (libgomp).
#
#  Libgomp is free software; you can redistribute it and/or modify it
#  under the terms of the GNU Lesser General Public License as published by
#  the Free Software Foundation; either version 2.1 of the License, or
#  (at your option) any later version.
#
#  Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
#  more details.
#
#  You should have received a copy of the GNU Lesser General Public License 
#  along with libgomp; see the file COPYING.LIB.  If not, write to the
#  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#
#  As a special exception, if you link this library with other files, some
#  of which are compiled with GCC, to produce an executable, this library
#  does not by itself cause the resulting executable to be covered by the
#  GNU General Public License.  This exception does not however invalidate
#  any other reasons why the executable file might be covered by the GNU
#  General Public License.

# This file fills in the blanks for omp.h.in, computing the size and
# alignment of the lock types that we export.  We do this to avoid
# polluting the namespace with e.g. pthreads declarations.

$COMPILE = $ARGV[0];
$INFILE = $ARGV[1];
$OUTFILE = $ARGV[2];

$HEADER = "#include \"omp-lock.h\"\n";

# configure might put libtool specific options into $COMPILE.
$COMPILE =~ s/ -Wc,/ /g;

# Close stderr in order to discard compiler errors.  Which we expect apleanty.
close STDERR;

# Return true if the boolean expression evaluates true at compile-time.
sub compile_test {
	my $expr = shift;

	open GCC, "|$COMPILE -fsyntax-only -xc -";
	print GCC $HEADER;
	print GCC "char test[($expr) ? 1 : -1];\n";
	return close GCC;
}

# Return a number guaranteed to be larger than the integer epression.
sub upper_bound {
	use integer;
	my $expr = shift;
	my $max = 9;

	while (compile_test("($expr) >= $max")) {
		$max = $max * 2;
	}

	return $max;
}

# Return an exact number for the integer expression.
sub binary_search {
	use integer;
	my $expr = shift;
	my $low = 1;
	my $high = upper_bound($expr);

	while ($low < $high) {
		my $mid = ($high + $low + 1) / 2;
		if (compile_test("($expr) >= $mid")) {
			$low = $mid;
		} else {
			$high = $mid - 1;
		}
	}

	return $low;
}

# Apply OP to TYPE, where OP is either sizeof or __alignof.
sub resolve {
	my $op = shift;
	my $type = shift;

	return binary_search("$op($type)");
}

# Find all the constants we need.
$sizeof_omp_lock_t = resolve ("sizeof", "omp_lock_t");
$alignof_omp_lock_t = resolve ("__alignof", "omp_lock_t");
$sizeof_omp_nest_lock_t = resolve ("sizeof", "omp_nest_lock_t");
$alignof_omp_nest_lock_t = resolve ("__alignof", "omp_nest_lock_t");
$omp_lock_kind = $sizeof_omp_lock_t;
$omp_nest_lock_kind = $sizeof_omp_nest_lock_t;
if ($sizeof_omp_lock_t >= 8 || $alignof_omp_lock_t > $sizeof_omp_lock_t) {
	$omp_lock_kind = 8;
}
if ($sizeof_omp_nest_lock_t >= 8 || $alignof_omp_nest_lock_t > $sizeof_omp_nest_lock_t) {
	$omp_nest_lock_kind = 8;
}

# Edit the input template into the output.
open IN, "<", $INFILE;
open OUT, ">", $OUTFILE;
while (<IN>) {
	s/OMP_LOCK_SIZE/$sizeof_omp_lock_t/o;
	s/OMP_LOCK_ALIGN/$alignof_omp_lock_t/o;
	s/OMP_NEST_LOCK_SIZE/$sizeof_omp_nest_lock_t/o;
	s/OMP_NEST_LOCK_ALIGN/$alignof_omp_nest_lock_t/o;
	s/OMP_LOCK_KIND/$omp_lock_kind/o;
	s/OMP_NEST_LOCK_KIND/$omp_nest_lock_kind/o;
	print OUT;
}

close OUT;
