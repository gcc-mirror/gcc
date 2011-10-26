#!/usr/bin/perl -w
#
# Define linkage sections required by GUPC by editing
# the default GNU ld script.
#
use strict;
my $before_ctors = <<EOD;
  .upc_alloc_array     :
  {
     PROVIDE (__upc_alloc_array_start = .);
     KEEP (*(SORT(upc_alloc_array.*)))
     KEEP (*(upc_alloc_array))
     PROVIDE (__upc_alloc_array_end = .);
  }
  .upc_alloc :  { KEEP(*(upc_alloc)) }
  .upc_init_array     :
  {
     PROVIDE (__upc_init_array_start = .);
     KEEP (*(SORT(upc_init_array.*)))
     KEEP (*(upc_init_array))
     PROVIDE (__upc_init_array_end = .);
  }
  .upc_init :  { KEEP(*(upc_init)) }
  /* UPC Program Info - compilation-related data */
  .upc_pgm_info : 
  {
    PROVIDE (__upc_pgm_info_start = .);
    KEEP (*(upc_pgm_info));
    PROVIDE (__upc_pgm_info_end = .);
  }
EOD
my $after_end_dot = <<EOD;
  /* UPC shared section - used to layout shared data only */
  .upc_shared 0x4000 (NOLOAD):
  {
    PROVIDE (__upc_shared_start = .);
    *(upc_shared);
    PROVIDE (__upc_shared_end = .);
  }
EOD
my $ld_script = do {local $/ = undef; <>};
for ($ld_script)
  {
    m{^GNU ld} or die "Not a GNU ld script?";
    my $is_ia64 = /OUTPUT_ARCH\s*\(\s*ia64\s*\)/s;
    if ($is_ia64)
      {
        # The linker on the IA64 (SuSE) can't handle the
	# additional "nolaod" attribute.  Drop it.
	$after_end_dot =~ s/\s*\(NOLOAD\)//s;
      }
    s/^.*?\n=+\n//s;
    s/\n(?:\s*\n)*=+\n.*$/\n/s;
    s/^(.*\n)(\s*\.ctors.*?\n)/$1$before_ctors$2/s
      or die "No match on .ctors line";
    m{(\n\s*\.\s*\=\s*ALIGN\s*\(\s*\d+\s*/\s*8\s*\)\s*;[^\n]*\n)}s;
    my $align = defined($1) ? "$1" : '';
    s{\n((?:\s*\.\s*\=\s*ALIGN\s*\(\s*\d+\s*/\s*\d+\s*\)\s*;[^\n]*\n)*?
	 \s*_end\s*=\s*\.\s*;
	 \s*PROVIDE\s*\(\s*end\s*=\s*\.\s*\)\s*;[^\n]*\n
	 (?:\s*\.\s*\=\s*DATA_SEGMENT_END\s*\(\s*\.\s*\)\s*;[^\n]*\n)?)}
     {$1$after_end_dot}sx
      or die "No match on '_end = .;' line";
  }
print $ld_script;
