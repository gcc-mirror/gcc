/* Make ucnid.h from various sources.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Run this program as
   ./makeucnid ucnid.tab UnicodeData.txt DerivedNormalizationProps.txt \
      DerivedCoreProperties.txt > ucnid.h
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

enum {
  C99 = 1,
  CXX = 2,
  N99 = 4,
  C11 = 8,
  N11 = 16,
  CXX23 = 32,
  NXX23 = 64,
  all_languages = C99 | CXX | C11 | CXX23 | NXX23,
  not_NFC = 128,
  not_NFKC = 256,
  maybe_not_NFC = 512
};

#define NUM_CODE_POINTS 0x110000
#define MAX_CODE_POINT 0x10ffff

static unsigned flags[NUM_CODE_POINTS];
static unsigned int all_decomp[NUM_CODE_POINTS][2];
static unsigned int decomp[NUM_CODE_POINTS][2];
static unsigned char combining_value[NUM_CODE_POINTS];

/* Die!  */

static void
fail (const char *s)
{
  fprintf (stderr, "%s\n", s);
  exit (1);
}

/* Read ucnid.tab and set the flags for language versions in header[].  */

static void
read_ucnid (const char *fname)
{
  FILE *f = fopen (fname, "r");
  unsigned fl = 0;

  if (!f)
    fail ("opening ucnid.tab");
  for (;;)
    {
      char line[256];

      if (!fgets (line, sizeof (line), f))
	break;
      if (strcmp (line, "[C99]\n") == 0)
	fl = C99;
      else if (strcmp (line, "[C99DIG]\n") == 0)
	fl = C99|N99;
      else if (strcmp (line, "[CXX]\n") == 0)
	fl = CXX;
      else if (strcmp (line, "[C11]\n") == 0)
	fl = C11;
      else if (strcmp (line, "[C11NOSTART]\n") == 0)
	fl = C11|N11;
      else if (isxdigit (line[0]))
	{
	  char *l = line;
	  while (*l)
	    {
	      unsigned long start, end;
	      char *endptr;
	      start = strtoul (l, &endptr, 16);
	      if (endptr == l || (*endptr != '-' && ! isspace (*endptr)))
		fail ("parsing ucnid.tab [1]");
	      l = endptr;
	      if (*l != '-')
		end = start;
	      else
		{
		  end = strtoul (l + 1, &endptr, 16);
		  if (end < start)
		    fail ("parsing ucnid.tab, end before start");
		  l = endptr;
		  if (! isspace (*l))
		    fail ("parsing ucnid.tab, junk after range");
		}
	      while (isspace (*l))
		l++;
	      if (end > MAX_CODE_POINT)
		fail ("parsing ucnid.tab, end too large");
	      while (start <= end)
		flags[start++] |= fl;
	    }
	}
    }
  if (ferror (f))
    fail ("reading ucnid.tab");
  fclose (f);
}

/* Read UnicodeData.txt and fill in the 'decomp' table to be the
   decompositions of characters for which both the character
   decomposed and all the code points in the decomposition are valid
   for some supported language version, and the 'all_decomp' table to
   be the decompositions of all characters without those
   constraints.  */

static void
read_table (char *fname)
{
  FILE * f = fopen (fname, "r");

  if (!f)
    fail ("opening UnicodeData.txt");
  for (;;)
    {
      char line[256];
      unsigned long codepoint, this_decomp[4];
      char *l;
      int i, j;
      int decomp_useful;

      if (!fgets (line, sizeof (line), f))
	break;
      codepoint = strtoul (line, &l, 16);
      if (l == line || *l != ';')
	fail ("parsing UnicodeData.txt, reading code point");
      if (codepoint > MAX_CODE_POINT)
	fail ("parsing UnicodeData.txt, code point too large");

      do {
	l++;
      } while (*l != ';');
      /* Category value.  */
      do {
	l++;
      } while (*l != ';');
      /* Canonical combining class; in NFC/NFKC, they must be increasing
	 (or zero).  */
      if (! isdigit (*++l))
	fail ("parsing UnicodeData.txt, combining class not number");
      combining_value[codepoint] = strtoul (l, &l, 10);
      if (*l++ != ';')
	fail ("parsing UnicodeData.txt, junk after combining class");

      /* Skip over bidi value.  */
      do {
	l++;
      } while (*l != ';');

      /* Decomposition mapping.  */
      decomp_useful = flags[codepoint];
      if (*++l == '<')  /* Compatibility mapping. */
	continue;
      for (i = 0; i < 4; i++)
	{
	  if (*l == ';')
	    break;
	  if (!isxdigit (*l))
	    fail ("parsing UnicodeData.txt, decomposition format");
	  this_decomp[i] = strtoul (l, &l, 16);
	  decomp_useful &= flags[this_decomp[i]];
	  while (isspace (*l))
	    l++;
	}
      if (i > 2)  /* Decomposition too long.  */
	fail ("parsing UnicodeData.txt, decomposition too long");
      for (j = 0; j < i; j++)
	all_decomp[codepoint][j] = this_decomp[j];
      if ((flags[codepoint] & all_languages) && decomp_useful)
	while (--i >= 0)
	  decomp[codepoint][i] = this_decomp[i];
    }
  if (ferror (f))
    fail ("reading UnicodeData.txt");
  fclose (f);
}

/* Read DerivedNormalizationProps.txt and set the flags that say whether
   a character is in NFC, NFKC, or is context-dependent.  */

static void
read_derived (const char *fname)
{
  FILE * f = fopen (fname, "r");

  if (!f)
    fail ("opening DerivedNormalizationProps.txt");
  for (;;)
    {
      char line[256];
      unsigned long start, end;
      char *l;
      bool not_NFC_p, not_NFKC_p, maybe_not_NFC_p;

      if (!fgets (line, sizeof (line), f))
	break;
      not_NFC_p = (strstr (line, "; NFC_QC; N") != NULL);
      not_NFKC_p = (strstr (line, "; NFKC_QC; N") != NULL);
      maybe_not_NFC_p = (strstr (line, "; NFC_QC; M") != NULL);
      if (! not_NFC_p && ! not_NFKC_p && ! maybe_not_NFC_p)
	continue;

      start = strtoul (line, &l, 16);
      if (l == line)
	fail ("parsing DerivedNormalizationProps.txt, reading start");
      if (start > MAX_CODE_POINT)
	fail ("parsing DerivedNormalizationProps.txt, code point too large");
      if (*l == '.' && l[1] == '.')
	end = strtoul (l + 2, &l, 16);
      else
	end = start;

      while (start <= end)
	flags[start++] |= ((not_NFC_p ? not_NFC : 0)
			   | (not_NFKC_p ? not_NFKC : 0)
			   | (maybe_not_NFC_p ? maybe_not_NFC : 0)
			   );
    }
  if (ferror (f))
    fail ("reading DerivedNormalizationProps.txt");
  fclose (f);
}

/* Read DerivedCoreProperties.txt and fill in languages version in
   flags from the XID_Start and XID_Continue properties.  */

static void
read_derivedcore (char *fname)
{
  FILE * f = fopen (fname, "r");

  if (!f)
    fail ("opening DerivedCoreProperties.txt");
  for (;;)
    {
      char line[256];
      unsigned long codepoint_start, codepoint_end;
      char *l;
      int i, j;

      if (!fgets (line, sizeof (line), f))
	break;
      if (line[0] == '#' || line[0] == '\n' || line[0] == '\r')
	continue;
      codepoint_start = strtoul (line, &l, 16);
      if (l == line)
	fail ("parsing DerivedCoreProperties.txt, reading code point");
      if (codepoint_start > MAX_CODE_POINT)
	fail ("parsing DerivedCoreProperties.txt, code point too large");

      if (*l == '.' && l[1] == '.')
	{
	  char *l2 = l + 2;
	  codepoint_end = strtoul (l + 2, &l, 16);
	  if (l == l2 || codepoint_end < codepoint_start)
	    fail ("parsing DerivedCoreProperties.txt, reading code point");
	  if (codepoint_end > MAX_CODE_POINT)
	    fail ("parsing DerivedCoreProperties.txt, code point too large");
	}
      else
	codepoint_end = codepoint_start;

      while (*l == ' ')
	l++;
      if (*l++ != ';')
	fail ("parsing DerivedCoreProperties.txt, reading code point");

      while (*l == ' ')
	l++;

      if (codepoint_end < 0x80)
        continue;

      if (strncmp (l, "XID_Start ", 10) == 0)
	{
	  for (; codepoint_start <= codepoint_end; codepoint_start++)
	    flags[codepoint_start]
	      = (flags[codepoint_start] | CXX23) & ~NXX23;
	}
      else if (strncmp (l, "XID_Continue ", 13) == 0)
	{
	  for (; codepoint_start <= codepoint_end; codepoint_start++)
	    if ((flags[codepoint_start] & CXX23) == 0)
	      flags[codepoint_start] |= CXX23 | NXX23;
	}
    }
  if (ferror (f))
    fail ("reading DerivedCoreProperties.txt");
  fclose (f);
}

/* Write out the table.
   The table consists of two words per entry.  The first word is the flags
   for the unicode code points up to and including the second word.  */

static void
write_table (void)
{
  unsigned i;
  unsigned last_flag = flags[0];
  bool really_safe = decomp[0][0] == 0;
  unsigned char last_combine = combining_value[0];

  printf ("static const struct ucnrange ucnranges[] = {\n");

  for (i = 1; i <= NUM_CODE_POINTS; i++)
    if (i == NUM_CODE_POINTS
	|| (flags[i] != last_flag && ((flags[i] | last_flag) & all_languages))
	|| really_safe != (decomp[i][0] == 0)
	|| combining_value[i] != last_combine)
      {
	printf ("{ %s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s, %3d, %#06x },\n",
		last_flag & C99 ? "C99" : "  0",
		last_flag & N99 ? "N99" : "  0",
		last_flag & CXX ? "CXX" : "  0",
		last_flag & C11 ? "C11" : "  0",
		last_flag & N11 ? "N11" : "  0",
		last_flag & CXX23 ? "CXX23" : "    0",
		last_flag & NXX23 ? "NXX23" : "    0",
		really_safe ? "CID" : "  0",
		last_flag & not_NFC ? "  0" : "NFC",
		last_flag & not_NFKC ? "  0" : "NKC",
		last_flag & maybe_not_NFC ? "CTX" : "  0",
		combining_value[i - 1],
		i - 1);
	last_flag = flags[i];
	last_combine = combining_value[i];
	really_safe = decomp[i][0] == 0;
      }

  printf ("};\n");
}

/* Return whether a given character is valid in an identifier for some
   supported language, either as itself or as a UCN.  */

static bool
char_id_valid (unsigned int c)
{
  return ((flags[c] & all_languages)
	  || (c == 0x24)
	  || (c >= 0x30 && c <= 0x39)
	  || (c >= 0x41 && c <= 0x5a)
	  || (c >= 0x61 && c <= 0x7a));
}

/* Write out the switch statement over characters for which it is
   context-dependent whether they are in NFC.  */

static void
write_context_switch (void)
{
  unsigned i;
  printf ("static bool\n"
	  "check_nfc (cpp_reader *pfile, cppchar_t c, cppchar_t p)\n"
	  "{\n"
	  "  switch (c)\n"
	  "    {\n");
  for (i = 0; i < NUM_CODE_POINTS; i++)
    {
      bool found_case = false;
      unsigned j;
      if (!(flags[i] & all_languages) || !(flags[i] & maybe_not_NFC))
	continue;
      if ((i >= 0x1161 && i <= 0x1175) || (i >= 0x11A8 && i <= 0x11C2))
	continue; /* Hangul handled algorithmically.  */
      printf ("    case %#06x:\n"
	      "      switch (p)\n"
	      "\t{\n", i);
      /* If an NFC starter character decomposes with this character I
	 as the second character and an NFC starter character S as the
	 first character, that latter character as a previous
	 character means this character is not NFC.  Furthermore, any
	 NFC starter character K made by a series of compositions of S
	 with combining characters whose combining class is greater
	 than that of I also means this character is not NFC.  */
      for (j = 0; j < NUM_CODE_POINTS; j++)
	{
	  unsigned s, k;
	  if (all_decomp[j][1] != i)
	    continue;
	  s = all_decomp[j][0];
	  if (combining_value[s] != 0 || (flags[s] & not_NFC) != 0)
	    continue;
	  if (char_id_valid (s))
	    {
	      found_case = true;
	      printf ("\tcase %#06x:\n", s);
	    }
	  for (k = 0; k < NUM_CODE_POINTS; k++)
	    {
	      unsigned t = k;
	      if (k == s || !char_id_valid (k))
		continue;
	      while (all_decomp[t][1] != 0
		     && combining_value[all_decomp[t][1]] > combining_value[i])
		{
		  if (combining_value[t] != 0 || (flags[t] & not_NFC) != 0)
		    break;
		  t = all_decomp[t][0];
		}
	      if (t == s)
		{
		  found_case = true;
		  printf ("\tcase %#06x:\n", k);
		}
	    }
	}
      if (found_case)
	printf ("\t  return false;\n");
      else
	printf ("\t/* Non-NFC cases not applicable to C/C++.  */\n");
      printf ("\tdefault:\n"
	      "\t  return true;\n"
	      "\t}\n\n");
    }
  printf ("    default:\n"
	  "      cpp_error (pfile, CPP_DL_ICE, \"Character %%x might not be NFKC\", c);\n"
	  "      return true;\n"
	  "  }\n"
	  "}\n");
}

/* Print out the huge copyright notice.  */

static void
write_copyright (void)
{
  static const char copyright[] = "\
/* Unicode characters and various properties.\n\
   Copyright (C) 2003-2025 Free Software Foundation, Inc.\n\
\n\
   This program is free software; you can redistribute it and/or modify it\n\
   under the terms of the GNU General Public License as published by the\n\
   Free Software Foundation; either version 3, or (at your option) any\n\
   later version.\n\
\n\
   This program is distributed in the hope that it will be useful,\n\
   but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
   GNU General Public License for more details.\n\
\n\
   You should have received a copy of the GNU General Public License\n\
   along with this program; see the file COPYING3.  If not see\n\
   <http://www.gnu.org/licenses/>.\n\
\n\
\n\
   Copyright (C) 1991-2024 Unicode, Inc.  All rights reserved.\n\
   Distributed under the Terms of Use in\n\
   http://www.unicode.org/copyright.html.\n\
\n\
   Permission is hereby granted, free of charge, to any person\n\
   obtaining a copy of the Unicode data files and any associated\n\
   documentation (the \"Data Files\") or Unicode software and any\n\
   associated documentation (the \"Software\") to deal in the Data Files\n\
   or Software without restriction, including without limitation the\n\
   rights to use, copy, modify, merge, publish, distribute, and/or\n\
   sell copies of the Data Files or Software, and to permit persons to\n\
   whom the Data Files or Software are furnished to do so, provided\n\
   that (a) the above copyright notice(s) and this permission notice\n\
   appear with all copies of the Data Files or Software, (b) both the\n\
   above copyright notice(s) and this permission notice appear in\n\
   associated documentation, and (c) there is clear notice in each\n\
   modified Data File or in the Software as well as in the\n\
   documentation associated with the Data File(s) or Software that the\n\
   data or software has been modified.\n\
\n\
   THE DATA FILES AND SOFTWARE ARE PROVIDED \"AS IS\", WITHOUT WARRANTY\n\
   OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE\n\
   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\n\
   NONINFRINGEMENT OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE\n\
   COPYRIGHT HOLDER OR HOLDERS INCLUDED IN THIS NOTICE BE LIABLE FOR\n\
   ANY CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY\n\
   DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,\n\
   WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS\n\
   ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE\n\
   OF THE DATA FILES OR SOFTWARE.\n\
\n\
   Except as contained in this notice, the name of a copyright holder\n\
   shall not be used in advertising or otherwise to promote the sale,\n\
   use or other dealings in these Data Files or Software without prior\n\
   written authorization of the copyright holder.  */\n";

   puts (copyright);
}

/* Main program.  */

int
main(int argc, char ** argv)
{
  if (argc != 5)
    fail ("too few arguments to makeucn");
  read_ucnid (argv[1]);
  read_table (argv[2]);
  read_derived (argv[3]);
  read_derivedcore (argv[4]);

  write_copyright ();
  write_table ();
  write_context_switch ();
  return 0;
}
