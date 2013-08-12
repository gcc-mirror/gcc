/*
This script sums up the counters for seeing how many virtual calls are
actually being verified.  The flag for generating the count data is
"-fvtv-counts".  This flag will generate two files in /tmp,
"vtv_count_data.log" and "vtv_class_set_sizes.log".  The first file is
the one that contains the info I mentioned; the second one is one I
generated because I was curious about how big the average set size was
for the vtable verification work.

After compiling the attached program, run it on the vtv_count_data.log
file:

$ sum-counters /tmp/vtv_count_data.log

One can optionally pass a "--verbose" flag.  This file generates an
output file whose name is the same as the input file, with ".summary"
appended to it, e.g. /tmp/vtv_count_data.log.summary .  Without the
verbose flag, it will just contain something like this:

Total # virtual calls: 349123
Total # verified calls: 348236
Percent verified: 99 %

Total calls to __VLTRegisterSet: 42236
Total calls to __VLTRegisterPair: 84371
Total # unused vtable map vars: 1536333

With the --verbose flag it will also output one line for each
compilation unit for which it verified less than 90% of the virtual
calls (and there were more than 20 virtual calls in the file),
something like this:

Verified 1 out of 25   (4.00%) :  foo.cc
Verified 27 out of 43   (62.00%) : bar.cc
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void
usage (const char *error_text)
{
  fprintf (stderr, "%s", error_text);
  fprintf (stderr, "Usage: \n");
  fprintf (stderr, "sum-counters <input-file> [--verbose]\n");
}

int
main (int argc, char **argv)
{
  FILE *fp_in = NULL;
  FILE *fp_out = NULL;
  int sum_vcalls = 0;
  int sum_verified = 0;
  int sum_regset = 0;
  int sum_regpair = 0;
  int sum_unused = 0;
  char fname_in[1024];
  char fname_out[1024];
  int total;
  int verified;
  int regset;
  int regpair;
  int unused;
  float pct;
  char buffer[1024];
  int verbose = 0;

  if (argc < 2)
    {
      usage ("Error: Need an input file.\n");
      return 1;
    }

  fp_in = fopen (argv[1], "r");
  if (!fp_in)
    {
      snprintf (buffer, 1024, "Error: Unable to open input file '%s'.\n",
		argv[1]);
      usage (buffer);
      return 1;
    }

  if (argc == 3)
    {
      if (strcmp (argv[2], "--verbose") == 0)
	verbose = 1;
      else
	{
	  snprintf (buffer, 1024, "Error: Unrecognized option '%s'.\n",
		    argv[2]);
	  usage (buffer);
	  return 1;
	}
    }

  snprintf (fname_out, 1024, "%s.summary", argv[1]);

  fp_out = fopen (fname_out, "w");
  if (!fp_out)
    {
      fprintf (stderr, "Error: Unable to open output file '%s'\n",
	       fname_out);
      return 1;
    }

  while (fscanf (fp_in, "%s %d %d %d %d %d\n", fname_in,  &total,
		 &verified, &regset, &regpair, &unused) != EOF)
    {
      sum_vcalls += total;
      sum_verified += verified;
      sum_regset += regset;
      sum_regpair += regpair;
      sum_unused += unused;

      float tmp_pct = 0.0;

      if (total > 0)
	tmp_pct = (verified * 100) / total;

      if (verbose && tmp_pct < 90 && total >= 20)
	{
	  fprintf (fp_out, "Verified %d out of %d   (%.2f%%) : %s\n",
		   verified, total, tmp_pct, fname_in);
	}

    }

  fclose (fp_in);

  fprintf (fp_out, "\n\n");
  fprintf (fp_out, "Total # virtual calls: %d\n", sum_vcalls);
  fprintf (fp_out, "Total # verified calls: %d\n", sum_verified);
  if (sum_vcalls > 0)
    fprintf (fp_out, "Percent verified: %d %%\n",
	     sum_verified * 100 / sum_vcalls);
  else
    fprintf (fp_out, "Percent verified: NA %%\n");
    
  fprintf (fp_out, "\nTotal calls to __VLTRegisterSet: %d\n",
	   sum_regset);
  fprintf (fp_out, "Total calls to __VLTRegisterPair: %d\n",
	   sum_regpair);
  fprintf (fp_out, "Total # unused vtable map vars: %d\n", sum_unused);

  fclose (fp_out);

  return 0;
}
