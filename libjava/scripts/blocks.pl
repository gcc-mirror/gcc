#! /usr/bin/perl

if ($ARGV[0] eq '')
{
    $file = 'Blocks.txt';
    if (! -f $file)
    {
	# Too painful to figure out how to get Perl to do it.
	system 'wget -o .wget-log http://www.unicode.org/Public/UNIDATA/Blocks.txt';
    }
}
else
{
    $file = $ARGV[0];
}

open (INPUT, "< $file") || die "couldn't open $file: $!";

@array = ();
while (<INPUT>)
{
    next if /^#/;
    chop;
    next if /^$/;

    ($start, $to, $text) = split (/; /);
    ($symbol = $text) =~ tr/a-z/A-Z/;
    $symbol =~ s/[- ]/_/g;

    # Special case for one of the SPECIALS.
    next if $start eq 'FEFF';

    # Special case some areas that our heuristic mishandles.
    if ($symbol eq 'HIGH_SURROGATES')
    {
	$symbol = 'SURROGATES_AREA';
	$text = 'Surrogates Area';
	$to = 'DFFF';
    }
    elsif ($symbol =~ /SURROGATES/)
    {
	next;
    }
    elsif ($symbol eq 'PRIVATE_USE')
    {
	$symbol .= '_AREA';
	$text = 'Private Use Area';
    }

    printf "    public static final UnicodeBlock %s = new UnicodeBlock (\"%s\", '\\u%s', '\\u%s');\n",
           $symbol, $text, $start, $to;

    push (@array, $symbol);
}

printf "    private static final UnicodeBlock[] blocks = {\n";
foreach (@array)
{
    printf "      %s", $_;
    printf "," unless $_ eq 'SPECIALS';
    printf "\n";
}
printf "    };\n";

close (INPUT);
