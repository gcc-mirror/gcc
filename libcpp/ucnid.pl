#! /usr/bin/perl -w
use strict;

# Convert cppucnid.tab to cppucnid.h.  We use two arrays of length
# 65536 to represent the table, since this is nice and simple.  The
# first array holds the tags indicating which ranges are valid in
# which contexts.  The second array holds the language name associated
# with each element.

our(@tags, @names);
@tags = ("") x 65536;
@names = ("") x 65536;


# Array mapping tag numbers to standard #defines
our @stds;

# Current standard and language
our($curstd, $curlang);

# First block of the file is a template to be saved for later.
our @template;

while (<>) {
    chomp;
    last if $_ eq '%%';
    push @template, $_;
};

# Second block of the file is the UCN tables.
# The format looks like this:
#
# [std]
#
# ; language
# xxxx-xxxx xxxx xxxx-xxxx ....
#
# with comment lines starting with #.

while (<>) {
    chomp;
    /^#/ and next;
    /^\s*$/ and next;
    /^\[(.+)\]$/ and do {
	$curstd = $1;
 	next;
    };
    /^; (.+)$/ and do {
	$curlang = $1;
	next;
    };

    process_range(split);
}

# Print out the template, inserting as requested.
$\ = "\n";
for (@template) {
    print("/* Automatically generated from cppucnid.tab, do not edit */"),
        next if $_ eq "[dne]";
    print_table(), next if $_ eq "[table]";
    print;
}

sub print_table {
    my($lo, $hi);
    my $prevname = "";

    for ($lo = 0; $lo <= $#tags; $lo = $hi) {
	$hi = $lo;
	$hi++ while $hi <= $#tags
	    && $tags[$hi] eq $tags[$lo]
	    && $names[$hi] eq $names[$lo];

	# Range from $lo to $hi-1.
	# Don't make entries for ranges that are not valid idchars.
	next if ($tags[$lo] eq "");
	my $tag = $tags[$lo];
        $tag = "    ".$tag if $tag =~ /^C99/;

	if ($names[$lo] eq $prevname) {
	    printf("  { 0x%04x, 0x%04x, %-11s },\n",
		   $lo, $hi-1, $tag);
	} else {
	    printf("  { 0x%04x, 0x%04x, %-11s },  /* %s */\n",
		   $lo, $hi-1, $tag, $names[$lo]);
	}
	$prevname = $names[$lo];
    }
}

# The line is a list of four-digit hexadecimal numbers or
# pairs of such numbers.  Each is a valid identifier character
# from the given language, under the given standard.
sub process_range {
    for my $range (@_) {
	if ($range =~ /^[0-9a-f]{4}$/) {
	    my $i = hex($range);
	    if ($tags[$i] eq "") {
		$tags[$i] = $curstd;
	    } else {
		$tags[$i] = $curstd . "|" . $tags[$i];
	    }
	    if ($names[$i] ne "" && $names[$i] ne $curlang) {
		warn sprintf ("language overlap: %s/%s at %x (tag %d)",
			      $names[$i], $curlang, $i, $tags[$i]);
		next;
	    }
	    $names[$i] = $curlang;
	} elsif ($range =~ /^ ([0-9a-f]{4}) - ([0-9a-f]{4}) $/x) {
	    my ($start, $end) = (hex($1), hex($2));
	    my $i;
	    for ($i = $start; $i <= $end; $i++) {
		if ($tags[$i] eq "") {
		    $tags[$i] = $curstd;
		} else {
		    $tags[$i] = $curstd . "|" . $tags[$i];
		}
		if ($names[$i] ne "" && $names[$i] ne $curlang) {
		    warn sprintf ("language overlap: %s/%s at %x (tag %d)",
				  $names[$i], $curlang, $i, $tags[$i]);
		    next;
		}
		$names[$i] = $curlang;
	    }
	} else {
	    warn "malformed range expression $range";
	}
    }
}
