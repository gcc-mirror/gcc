#!/usr/bin/perl -w

# Copyright (C) 2010, 2011 Free Software Foundation, Inc.
#
# This file is part of the GNU ISO C++ Library.  This library is free
# software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this library; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Extract symbol version information on Solaris 2.
#
# Sun ld doesn't record symbol versions in .dynsym entries and they cannot
# easily be extracted from readelf --versions output, so use pvs instead.
# This way, we don't require GNU binutils in the native case.  Also ensures
# that baseline_symbols.txt is identical between native (pvs, elfdump) and
# cross (readelf) cases.

my $lib = shift;

open PVS, "pvs -dsvo $lib |" or die $!;
while (<PVS>) {
    chomp;

    # Remove trailing semicolon.
    s/;$//;

    # shared object, dash, version, symbol, [size]
    (undef, undef, $version, $symbol, $size) = split;

    # Remove colon separator from version field.
    $version =~ s/:$//;
    
    # Record base version.  The [BASE] field was only added in Solaris 11,
    # so simply use the first record instead.
    if ($. == 1) {
	$basever = $version;
      	next;
    }

    # Skip version declarations.
    next unless defined ($symbol);

    # Ignore version dependencies.
    next if ($symbol =~ /\{.*\}/);

    # Emit objects.
    if (defined ($size)) {
	# Strip parens from object size.
	$size =~ s/\((\d+)\)/$1/;

	$type{$symbol} = "OBJECT";
	$version{$symbol} = $version;
	$size{$symbol} = $size;
        next;
    }

    if ($version eq $symbol or $version eq $basever) {
	# Emit versions or symbols bound to base versions as objects.
	$type{$symbol} = "OBJECT";
	$version{$symbol} = $symbol;
	$size{$symbol} = 0;
    } else {
	# Everything else without a size field is a function.
	$type{$symbol} = "FUNC";
	$version{$symbol} = $version;
    }
}
close PVS or die "pvs error";

# Only look at .dynsym table, like readelf in extract_symvers.
open ELFDUMP, "/usr/ccs/bin/elfdump -s -N .dynsym $lib |" or die $!;
while (<ELFDUMP>) {
    chomp;

    # Ignore empty lines.
    next if (/^$/);

    # Ignore object name header.
    next if (/:$/);

    # Ignore table header lines.
    next if (/^Symbol Table Section:/);
    next if (/index.*value.*size/);

    # Split table.
    (undef, undef, undef, $type, $bind, $oth, undef, $shndx, $name) = split;

    # Error out for unknown input.
    die "unknown input line:\n$_" unless defined($bind);

    # Ignore local symbols.
    next if ($bind eq "LOCL");
    # Ignore hidden symbols.
    next if ($oth eq "H");
    # Ignore undefined symbols.
    next if ($shndx eq "UNDEF");
    # Error out for unhandled cases.   _GLOBAL_OFFSET_TABLE_ is P (protected).
    die "unhandled symbol:\n$_" if ($bind !~ /^(GLOB|WEAK)/ or $oth !~ /[DP]/);

    # Adapt to readelf type naming convention.
    $type = "NOTYPE" if ($type eq "NOTY");
    $type = "OBJECT" if ($type eq "OBJT");

    # Use correct symbol type.
    $type{$name} = $type if ($type{$name} ne $type);
}
close ELFDUMP or die "elfdump error";

foreach $symbol (keys %type) {
    if ($type{$symbol} eq "FUNC" || $type{$symbol} eq "NOTYPE") {
	push @lines, "$type{$symbol}:$symbol\@\@$version{$symbol}\n";
    } elsif ($type{$symbol} eq "OBJECT" and $size{$symbol} == 0) {
	push @lines, "$type{$symbol}:$size{$symbol}:$version{$symbol}\n";
    } else {
	push @lines, "$type{$symbol}:$size{$symbol}:$symbol\@\@$version{$symbol}\n";
    }
}
print sort @lines;
