/* install-info -- create Info directory entry(ies) for an Info file.
   $Id: install-info.c,v 1.1.1.3 1998/03/24 18:20:30 law Exp $

   Copyright (C) 1996, 97, 98 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.*/

#include "system.h"
#include <getopt.h>

#ifdef HAVE_LIBZ
#include <zlib.h>
#endif

/* Name this program was invoked with.  */
char *progname;

char *readfile ();
struct line_data *findlines ();
void fatal ();
void insert_entry_here ();
int compare_section_names ();

struct spec_entry;

/* Data structures.  */


/* Record info about a single line from a file as read into core.  */
struct line_data
{
  /* The start of the line.  */
  char *start;
  /* The number of characters in the line,
     excluding the terminating newline.  */
  int size;
  /* Vector containing pointers to the entries to add before this line.
     The vector is null-terminated.  */
  struct spec_entry **add_entries_before;
  /* 1 means output any needed new sections before this line.  */
  int add_sections_before;
  /* 1 means don't output this line.  */
  int delete;
};


/* This is used for a list of the specified menu section names
   in which entries should be added.  */
struct spec_section
{
  struct spec_section *next;
  char *name;
  /* 1 means we have not yet found an existing section with this name
     in the dir file--so we will need to add a new section.  */
  int missing;
};


/* This is used for a list of the entries specified to be added.  */
struct spec_entry
{
  struct spec_entry *next;
  char *text;
};


/* This is used for a list of nodes found by parsing the dir file.  */
struct node
{
  struct node *next;
  /* The node name.  */
  char *name;
  /* The line number of the line where the node starts.
     This is the line that contains control-underscore.  */
  int start_line;
  /* The line number of the line where the node ends,
     which is the end of the file or where the next line starts.  */
  int end_line;
  /* Start of first line in this node's menu
     (the line after the * Menu: line).  */
  char *menu_start;
  /* The start of the chain of sections in this node's menu.  */
  struct menu_section *sections;
  /* The last menu section in the chain.  */
  struct menu_section *last_section;
};


/* This is used for a list of sections found in a node's menu.
   Each  struct node  has such a list in the  sections  field.  */
struct menu_section
{
  struct menu_section *next;
  char *name;
  /* Line number of start of section.  */
  int start_line;
  /* Line number of end of section.  */
  int end_line;
};

/* Memory allocation and string operations.  */

/* Like malloc but get fatal error if memory is exhausted.  */
void *
xmalloc (size)
     unsigned int size;
{
  extern void *malloc ();
  void *result = malloc (size);
  if (result == NULL)
    fatal (_("virtual memory exhausted"), 0);
  return result;
}

/* Like realloc but get fatal error if memory is exhausted.  */
void *
xrealloc (obj, size)
     void *obj;
     unsigned int size;
{
  extern void *realloc ();
  void *result = realloc (obj, size);
  if (result == NULL)
    fatal (_("virtual memory exhausted"), 0);
  return result;
}

/* Return a newly-allocated string
   whose contents concatenate those of S1, S2, S3.  */
char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

/* Return a string containing SIZE characters
   copied from starting at STRING.  */

char *
copy_string (string, size)
     char *string;
     int size;
{
  int i;
  char *copy = (char *) xmalloc (size + 1);
  for (i = 0; i < size; i++)
    copy[i] = string[i];
  copy[size] = 0;
  return copy;
}

/* Error message functions.  */

/* Print error message.  S1 is printf control string, S2 and S3 args for it. */

/* VARARGS1 */
void
error (s1, s2, s3)
     char *s1, *s2, *s3;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2, s3);
  putc ('\n', stderr);
}

/* VARARGS1 */
void
warning (s1, s2, s3)
     char *s1, *s2, *s3;
{
  fprintf (stderr, _("%s: warning: "), progname);
  fprintf (stderr, s1, s2, s3);
  putc ('\n', stderr);
}

/* Print error message and exit.  */

void
fatal (s1, s2, s3)
     char *s1, *s2, *s3;
{
  error (s1, s2, s3);
  exit (1);
}

/* Print fatal error message based on errno, with file name NAME.  */

void
pfatal_with_name (name)
     char *name;
{
  char *s = concat ("", strerror (errno), _(" for %s"));
  fatal (s, name);
}

/* Given the full text of a menu entry, null terminated,
   return just the menu item name (copied).  */

char *
extract_menu_item_name (item_text)
     char *item_text;
{
  char *p;

  if (*item_text == '*')
    item_text++;
  while (*item_text == ' ')
    item_text++;

  p = item_text;
  while (*p && *p != ':') p++;
  return copy_string (item_text, p - item_text);
}

/* Given the full text of a menu entry, terminated by null or newline,
   return just the menu item file (copied).  */

char *
extract_menu_file_name (item_text)
     char *item_text;
{
  char *p = item_text;

  /* If we have text that looks like * ITEM: (FILE)NODE...,
     extract just FILE.  Otherwise return "(none)".  */

  if (*p == '*')
    p++;
  while (*p == ' ')
    p++;

  /* Skip to and past the colon.  */
  while (*p && *p != '\n' && *p != ':') p++;
  if (*p == ':') p++;

  /* Skip past the open-paren.  */
  while (1)
    {
      if (*p == '(')
        break;
      else if (*p == ' ' || *p == '\t')
        p++;
      else
        return "(none)";
    }
  p++;

  item_text = p;

  /* File name ends just before the close-paren.  */
  while (*p && *p != '\n' && *p != ')') p++;
  if (*p != ')')
    return "(none)";

  return copy_string (item_text, p - item_text);
}

void
suggest_asking_for_help ()
{
  fprintf (stderr, _("\tTry `%s --help' for a complete list of options.\n"),
           progname);
  exit (1);
}

void
print_help ()
{
  printf (_("Usage: %s [OPTION]... [INFO-FILE [DIR-FILE]]\n\
\n\
Install INFO-FILE in the Info directory file DIR-FILE.\n\
\n\
Options:\n\
--delete          Delete existing entries in INFO-FILE;\n\
                    don't insert any new entries.\n\
--dir-file=NAME   Specify file name of Info directory file.\n\
                    This is equivalent to using the DIR-FILE argument.\n\
--entry=TEXT      Insert TEXT as an Info directory entry.\n\
                    TEXT should have the form of an Info menu item line\n\
                    plus zero or more extra lines starting with whitespace.\n\
                    If you specify more than one entry, they are all added.\n\
                    If you don't specify any entries, they are determined\n\
                    from information in the Info file itself.\n\
--help            Display this help and exit.\n\
--info-file=FILE  Specify Info file to install in the directory.\n\
                    This is equivalent to using the INFO-FILE argument.\n\
--info-dir=DIR    Same as --dir-file=DIR/dir.\n\
--item=TEXT       Same as --entry TEXT.\n\
                    An Info directory entry is actually a menu item.\n\
--quiet           Suppress warnings.\n\
--remove          Same as --delete.\n\
--section=SEC     Put this file's entries in section SEC of the directory.\n\
                    If you specify more than one section, all the entries\n\
                    are added in each of the sections.\n\
                    If you don't specify any sections, they are determined\n\
                    from information in the Info file itself.\n\
--version         Display version information and exit.\n\
\n\
Email bug reports to bug-texinfo@gnu.org.\n\
"), progname);
}


/* If DIRFILE does not exist, create a minimal one (or abort).  If it
   already exists, do nothing.  */

void
ensure_dirfile_exists (dirfile)
     char *dirfile;
{
  int desc = open (dirfile, O_RDONLY);
  if (desc < 0 && errno == ENOENT)
    {
      FILE *f;
      char *readerr = strerror (errno);
      close (desc);
      f = fopen (dirfile, "w");
      if (f)
        {
          fputs (_("This is the file .../info/dir, which contains the\n\
topmost node of the Info hierarchy, called (dir)Top.\n\
The first time you invoke Info you start off looking at this node.\n\
\n\
File: dir,\tNode: Top,\tThis is the top of the INFO tree\n\
\n\
  This (the Directory node) gives a menu of major topics.\n\
  Typing \"q\" exits, \"?\" lists all Info commands, \"d\" returns here,\n\
  \"h\" gives a primer for first-timers,\n\
  \"mEmacs<Return>\" visits the Emacs manual, etc.\n\
\n\
  In Emacs, you can click mouse button 2 on a menu item or cross reference\n\
  to select it.\n\
\n\
* Menu:\n\
"), f);
          if (fclose (f) < 0)
            pfatal_with_name (dirfile);
        }
      else
        {
          /* Didn't exist, but couldn't open for writing.  */
          fprintf (stderr,
                   _("%s: could not read (%s) and could not create (%s)\n"),
                   dirfile, readerr, strerror (errno));
          exit (1);
        }
    }
  else
    close (desc); /* It already existed, so fine.  */
}

/* This table defines all the long-named options, says whether they
   use an argument, and maps them into equivalent single-letter options.  */

struct option longopts[] =
{
  { "delete",    no_argument, NULL, 'r' },
  { "dir-file",  required_argument, NULL, 'd' },
  { "entry",     required_argument, NULL, 'e' },
  { "help",      no_argument, NULL, 'h' },
  { "info-dir",  required_argument, NULL, 'D' },
  { "info-file", required_argument, NULL, 'i' },
  { "item",      required_argument, NULL, 'e' },
  { "quiet",     no_argument, NULL, 'q' },
  { "remove",    no_argument, NULL, 'r' },
  { "section",   required_argument, NULL, 's' },
  { "version",   no_argument, NULL, 'V' },
  { 0 }
};


int
main (argc, argv)
     int argc;
     char **argv;
{
  char *infile = 0, *dirfile = 0;
  char *infile_sans_info;
  unsigned infilelen_sans_info;
  FILE *output;

  /* Record the text of the Info file, as a sequence of characters
     and as a sequence of lines.  */
  char *input_data;
  int input_size;
  struct line_data *input_lines;
  int input_nlines;

  /* Record here the specified section names and directory entries.  */
  struct spec_section *input_sections = NULL;
  struct spec_entry *entries_to_add = NULL;
  int n_entries_to_add = 0;

  /* Record the old text of the dir file, as plain characters,
     as lines, and as nodes.  */
  char *dir_data;
  int dir_size;
  int dir_nlines;
  struct line_data *dir_lines;
  struct node *dir_nodes;

  /* Nonzero means --delete was specified (just delete existing entries).  */
  int delete_flag = 0;
  int something_deleted = 0;
  /* Nonzero means -q was specified.  */
  int quiet_flag = 0;

  int node_header_flag;
  int prefix_length;
  int i;

  progname = argv[0];

#ifdef HAVE_SETLOCALE
  /* Set locale via LC_ALL.  */
  setlocale (LC_ALL, "");
#endif

  /* Set the text message domain.  */
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  while (1)
    {
      int opt = getopt_long (argc, argv, "i:d:e:s:hHr", longopts, 0);

      if (opt == EOF)
        break;

      switch (opt)
        {
        case 0:
          /* If getopt returns 0, then it has already processed a
             long-named option.  We should do nothing.  */
          break;

        case 1:
          abort ();

        case 'd':
          if (dirfile)
            {
              fprintf (stderr, _("%s: Specify the Info directory only once.\n"),
                       progname);
              suggest_asking_for_help ();
            }
          dirfile = optarg;
          break;

        case 'D':
          if (dirfile)
            {
              fprintf (stderr, _("%s: Specify the Info directory only once.\n"),
                       progname);
              suggest_asking_for_help ();
            }
          dirfile = concat (optarg, "", "/dir");
          break;

        case 'e':
          {
            struct spec_entry *next
              = (struct spec_entry *) xmalloc (sizeof (struct spec_entry));
            if (! (*optarg != 0 && optarg[strlen (optarg) - 1] == '\n'))
              optarg = concat (optarg, "\n", "");
            next->text = optarg;
            next->next = entries_to_add;
            entries_to_add = next;
            n_entries_to_add++;
          }
          break;

        case 'h':
        case 'H':
          print_help ();
          exit (0);

        case 'i':
          if (infile)
            {
              fprintf (stderr, _("%s: Specify the Info file only once.\n"),
                       progname);
              suggest_asking_for_help ();
            }
          infile = optarg;
          break;

        case 'q':
          quiet_flag = 1;
          break;

        case 'r':
          delete_flag = 1;
          break;

        case 's':
          {
            struct spec_section *next
              = (struct spec_section *) xmalloc (sizeof (struct spec_section));
            next->name = optarg;
            next->next = input_sections;
            next->missing = 1;
            input_sections = next;
          }
          break;

        case 'V':
          printf ("install-info (GNU %s) %s\n", PACKAGE, VERSION);
	  printf (_("Copyright (C) %s Free Software Foundation, Inc.\n\
There is NO warranty.  You may redistribute this software\n\
under the terms of the GNU General Public License.\n\
For more information about these matters, see the files named COPYING.\n"),
		  "1998");
          exit (0);

        default:
          suggest_asking_for_help ();
        }
    }

  /* Interpret the non-option arguments as file names.  */
  for (; optind < argc; ++optind)
    {
      if (infile == 0)
        infile = argv[optind];
      else if (dirfile == 0)
        dirfile = argv[optind];
      else
        error (_("excess command line argument `%s'"), argv[optind]);
    }

  if (!infile)
    fatal (_("No input file specified; try --help for more information."));
  if (!dirfile)
    fatal (_("No dir file specified; try --help for more information."));

  /* Read the Info file and parse it into lines.  */

  input_data = readfile (infile, &input_size);
  input_lines = findlines (input_data, input_size, &input_nlines);

  /* Parse the input file to find the section names it specifies.  */

  if (input_sections == 0)
    {
      prefix_length = strlen ("INFO-DIR-SECTION ");
      for (i = 0; i < input_nlines; i++)
        {
          if (!strncmp ("INFO-DIR-SECTION ", input_lines[i].start,
                        prefix_length))
            {
              struct spec_section *next
                = (struct spec_section *) xmalloc (sizeof (struct spec_section));
              next->name = copy_string (input_lines[i].start + prefix_length,
                                        input_lines[i].size - prefix_length);
              next->next = input_sections;
              next->missing = 1;
              input_sections = next;
            }
        }
    }

  /* Default to section "Miscellaneous" if no sections specified.  */
  if (input_sections == 0)
    {
      input_sections
        = (struct spec_section *) xmalloc (sizeof (struct spec_section));
      input_sections->name = "Miscellaneous";
      input_sections->next = 0;
      input_sections->missing = 1;
    }

  /* Now find the directory entries specified in the file
     and put them on entries_to_add.  But not if entries
     were specified explicitly with command options.  */

  if (entries_to_add == 0)
    {
      char *start_of_this_entry = 0;
      for (i = 0; i < input_nlines; i++)
        {
          if (!strncmp ("START-INFO-DIR-ENTRY", input_lines[i].start,
                        input_lines[i].size)
              && sizeof ("START-INFO-DIR-ENTRY") - 1 == input_lines[i].size)
            {
              if (start_of_this_entry != 0)
                fatal (_("START-INFO-DIR-ENTRY without matching END-INFO-DIR-ENTRY"));
              start_of_this_entry = input_lines[i + 1].start;
            }
          if (!strncmp ("END-INFO-DIR-ENTRY", input_lines[i].start,
                        input_lines[i].size)
              && sizeof ("END-INFO-DIR-ENTRY") - 1 == input_lines[i].size)
            {
              if (start_of_this_entry != 0)
                {
                  struct spec_entry *next
                    = (struct spec_entry *) xmalloc (sizeof (struct spec_entry));
                  next->text = copy_string (start_of_this_entry,
                                            input_lines[i].start - start_of_this_entry);
                  next->next = entries_to_add;
                  entries_to_add = next;
                  n_entries_to_add++;
                  start_of_this_entry = 0;
                }
              else
                fatal (_("END-INFO-DIR-ENTRY without matching START-INFO-DIR-ENTRY"));
            }
        }
      if (start_of_this_entry != 0)
        fatal (_("START-INFO-DIR-ENTRY without matching END-INFO-DIR-ENTRY"));
    }

  if (!delete_flag)
    if (entries_to_add == 0)
      { /* No need to abort here, the original info file may not have
           the requisite Texinfo commands.  This is not something an
           installer should have to correct (it's a problem for the
           maintainer), and there's no need to cause subsequent parts of
           `make install' to fail.  */
        warning (_("no info dir entry in `%s'"), infile);
        exit (0);
      }

  /* Now read in the Info dir file.  */
  ensure_dirfile_exists (dirfile);
  dir_data = readfile (dirfile, &dir_size);
  dir_lines = findlines (dir_data, dir_size, &dir_nlines);

  /* We will be comparing the entries in the dir file against the
     current filename, so need to strip off any directory prefix and any
     .info suffix.  */
  {
    unsigned basename_len;
    char *infile_basename = strrchr (infile, '/');
    if (infile_basename)
      infile_basename++;
    else
      infile_basename = infile;
    
    basename_len = strlen (infile_basename);
    infile_sans_info
      = (strlen (infile_basename) > 5
         && strcmp (infile_basename + basename_len - 5, ".info") == 0)
        ? copy_string (infile_basename, basename_len - 5)
        : infile_basename;

    infilelen_sans_info = strlen (infile_sans_info);
  }
  
  /* Parse the dir file.  Find all the nodes, and their menus,
     and the sections of their menus.  */

  dir_nodes = 0;
  node_header_flag = 0;
  for (i = 0; i < dir_nlines; i++)
    {
      /* Parse node header lines.  */
      if (node_header_flag)
        {
          int j, end;
          for (j = 0; j < dir_lines[i].size; j++)
            /* Find the node name and store it in the `struct node'.  */
            if (!strncmp ("Node:", dir_lines[i].start + j, 5))
              {
                char *line = dir_lines[i].start;
                /* Find the start of the node name.  */
                j += 5;
                while (line[j] == ' ' || line[j] == '\t')
                  j++;
                /* Find the end of the node name.  */
                end = j;
                while (line[end] != 0 && line[end] != ',' && line[end] != '\n'
                       && line[end] != '\t')
                  end++;
                dir_nodes->name = copy_string (line + j, end - j);
              }
          node_header_flag = 0;
        }

      /* Notice the start of a node.  */
      if (*dir_lines[i].start == 037)
        {
          struct node *next
            = (struct node *) xmalloc (sizeof (struct node));
          next->next = dir_nodes;
          next->name = NULL;
          next->start_line = i;
          next->end_line = 0;
          next->menu_start = NULL;
          next->sections = NULL;
          next->last_section = NULL;

          if (dir_nodes != 0)
            dir_nodes->end_line = i;
          /* Fill in the end of the last menu section
             of the previous node.  */
          if (dir_nodes != 0 && dir_nodes->last_section != 0)
            dir_nodes->last_section->end_line = i;

          dir_nodes = next;

          /* The following line is the header of this node;
             parse it.  */
          node_header_flag = 1;
        }

      /* Notice the lines that start menus.  */
      if (dir_nodes != 0
          && !strncmp ("* Menu:", dir_lines[i].start, 7))
        dir_nodes->menu_start = dir_lines[i + 1].start;

      /* Notice sections in menus.  */
      if (dir_nodes != 0
          && dir_nodes->menu_start != 0
          && *dir_lines[i].start != '\n'
          && *dir_lines[i].start != '*'
          && *dir_lines[i].start != ' '
          && *dir_lines[i].start != '\t')
        {
          /* Add this menu section to the node's list.
             This list grows in forward order.  */
          struct menu_section *next
            = (struct menu_section *) xmalloc (sizeof (struct menu_section));
          next->start_line = i + 1;
          next->next = 0;
          next->end_line = 0;
          next->name = copy_string (dir_lines[i].start, dir_lines[i].size);
          if (dir_nodes->sections)
            {
              dir_nodes->last_section->next = next;
              dir_nodes->last_section->end_line = i;
            }
          else
            dir_nodes->sections = next;
          dir_nodes->last_section = next;
        }

      /* Check for an existing entry that should be deleted.
         Delete all entries which specify this file name.  */
      if (*dir_lines[i].start == '*')
        {
          char *p = dir_lines[i].start;

          while (*p != 0 && *p != ':')
            p++;
          p++;
          while (*p == ' ') p++;
          if (*p == '(')
            {
              p++;
              if ((dir_lines[i].size
                   > (p - dir_lines[i].start + infilelen_sans_info))
                  && !strncmp (p, infile_sans_info, infilelen_sans_info)
                  && (p[infilelen_sans_info] == ')'
                      || !strncmp (p + infilelen_sans_info, ".info)", 6)))
                {
                  dir_lines[i].delete = 1;
                  something_deleted = 1;
                }
            }
        }
      /* Treat lines that start with whitespace
         as continuations; if we are deleting an entry,
         delete all its continuations as well.  */
      else if (i > 0
               && (*dir_lines[i].start == ' '
                   || *dir_lines[i].start == '\t'))
        {
          dir_lines[i].delete = dir_lines[i - 1].delete;
          something_deleted = 1;
        }
    }

  /* Finish the info about the end of the last node.  */
  if (dir_nodes != 0)
    {
      dir_nodes->end_line = dir_nlines;
      if (dir_nodes->last_section != 0)
        dir_nodes->last_section->end_line = dir_nlines;
    }

  /* Decide where to add the new entries (unless --delete was used).
     Find the menu sections to add them in.
     In each section, find the proper alphabetical place to add
     each of the entries.  */

  if (!delete_flag)
    {
      struct node *node;
      struct menu_section *section;
      struct spec_section *spec;

      for (node = dir_nodes; node; node = node->next)
        for (section = node->sections; section; section = section->next)
          {
            for (i = section->end_line; i > section->start_line; i--)
              if (dir_lines[i - 1].size != 0)
                break;
            section->end_line = i;

            for (spec = input_sections; spec; spec = spec->next)
              if (!strcmp (spec->name, section->name))
                break;
            if (spec)
              {
                int add_at_line = section->end_line;
                struct spec_entry *entry;
                /* Say we have found at least one section with this name,
                   so we need not add such a section.  */
                spec->missing = 0;
                /* For each entry, find the right place in this section
                   to add it.  */
                for (entry = entries_to_add; entry; entry = entry->next)
                  {
                    int textlen = strlen (entry->text);
                    /* Subtract one because dir_lines is zero-based,
                       but the `end_line' and `start_line' members are
                       one-based.  */
                    for (i = section->end_line - 1;
                         i >= section->start_line - 1; i--)
                      {
                        /* If an entry exists with the same name,
                           and was not marked for deletion
                           (which means it is for some other file),
                           we are in trouble.  */
                        if (dir_lines[i].start[0] == '*'
                            && menu_line_equal (entry->text, textlen,
                                                dir_lines[i].start,
                                                dir_lines[i].size)
                            && !dir_lines[i].delete)
                          fatal (_("menu item `%s' already exists, for file `%s'"),
                                 extract_menu_item_name (entry->text),
                                 extract_menu_file_name (dir_lines[i].start));
                        if (dir_lines[i].start[0] == '*'
                            && menu_line_lessp (entry->text, textlen,
                                                dir_lines[i].start,
                                                dir_lines[i].size))
                          add_at_line = i;
                      }
                    insert_entry_here (entry, add_at_line,
                                       dir_lines, n_entries_to_add);
                  }
              }
          }

      /* Mark the end of the Top node as the place to add any
         new sections that are needed.  */
      for (node = dir_nodes; node; node = node->next)
        if (node->name && strcmp (node->name, "Top") == 0)
          dir_lines[node->end_line].add_sections_before = 1;
    }

  if (delete_flag && !something_deleted && !quiet_flag)
    warning (_("no entries found for `%s'; nothing deleted"), infile);

  /* Output the old dir file, interpolating the new sections
     and/or new entries where appropriate.  */

  output = fopen (dirfile, "w");
  if (!output)
    {
      perror (dirfile);
      exit (1);
    }

  for (i = 0; i <= dir_nlines; i++)
    {
      int j;

      /* If we decided to output some new entries before this line,
         output them now.  */
      if (dir_lines[i].add_entries_before)
        for (j = 0; j < n_entries_to_add; j++)
          {
            struct spec_entry *this = dir_lines[i].add_entries_before[j];
            if (this == 0)
              break;
            fputs (this->text, output);
          }
      /* If we decided to add some sections here
         because there are no such sections in the file,
         output them now.  */
      if (dir_lines[i].add_sections_before)
        {
          struct spec_section *spec;
          struct spec_section **sections;
          int n_sections = 0;

          /* Count the sections and allocate a vector for all of them.  */
          for (spec = input_sections; spec; spec = spec->next)
            n_sections++;
          sections = ((struct spec_section **)
                      xmalloc (n_sections * sizeof (struct spec_section *)));

          /* Fill the vector SECTIONS with pointers to all the sections,
             and sort them.  */
          j = 0;
          for (spec = input_sections; spec; spec = spec->next)
            sections[j++] = spec;
          qsort (sections, n_sections, sizeof (struct spec_section *),
                 compare_section_names);

          /* Generate the new sections in alphabetical order.
             In each new section, output all of our entries.  */
          for (j = 0; j < n_sections; j++)
            {
              spec = sections[j];
              if (spec->missing)
                {
                  struct spec_entry *entry;

                  putc ('\n', output);
                  fputs (spec->name, output);
                  putc ('\n', output);
                  for (entry = entries_to_add; entry; entry = entry->next)
                    fputs (entry->text, output);
                }
            }

          free (sections);
        }

      /* Output the original dir lines unless marked for deletion.  */
      if (i < dir_nlines && !dir_lines[i].delete)
        {
          fwrite (dir_lines[i].start, 1, dir_lines[i].size, output);
          putc ('\n', output);
        }
    }

  fclose (output);

  exit (0);
}

/* Read all of file FILNAME into memory
   and return the address of the data.
   Store the size into SIZEP.
   If there is trouble, do a fatal error.  */

char *
readfile (filename, sizep)
     char *filename;
     int *sizep;
{
  int desc;
  int data_size = 1024;
  char *data = (char *) xmalloc (data_size);
  int filled = 0;
  int nread = 0;
#ifdef HAVE_LIBZ
  int isGZ = 0;
  gzFile zdesc;
#endif

  desc = open (filename, O_RDONLY);
  if (desc < 0)
    pfatal_with_name (filename);

#ifdef HAVE_LIBZ
  /* The file should always be two bytes long.  */
  if (read (desc, data, 2) != 2)
    pfatal_with_name (filename);

  /* Undo that read.  */
  lseek (desc, 0, SEEK_SET);

  /* If we see gzip magic, use gzdopen. */
  if (data[0] == '\x1f' && data[1] == '\x8b')
    {
      isGZ = 1;
      zdesc = gzdopen (desc, "r");
      if (zdesc == NULL) {
        close (desc);
        pfatal_with_name (filename);
      }
    }
#endif /* HAVE_LIBZ */

  while (1)
    {
#ifdef HAVE_LIBZ
      if (isGZ)
	nread = gzread (zdesc, data + filled, data_size - filled);
      else
#endif
        nread = read (desc, data + filled, data_size - filled);

      if (nread < 0)
        pfatal_with_name (filename);
      if (nread == 0)
        break;

      filled += nread;
      if (filled == data_size)
        {
          data_size *= 2;
          data = (char *) xrealloc (data, data_size);
        }
    }

  *sizep = filled;

#ifdef HAVE_LIBZ
  if (isGZ)
    gzclose (zdesc);
  else
#endif
    close(desc);

  return data;
}

/* Divide the text at DATA (of SIZE bytes) into lines.
   Return a vector of struct line_data describing the lines.
   Store the length of that vector into *NLINESP.  */

struct line_data *
findlines (data, size, nlinesp)
     char *data;
     int size;
     int *nlinesp;
{
  struct line_data *lines;
  int lines_allocated = 512;
  int filled = 0;
  int i = 0;
  int lineflag;

  lines = (struct line_data *) xmalloc (lines_allocated * sizeof (struct line_data));

  lineflag = 1;
  for (i = 0; i < size; i++)
    {
      if (lineflag)
        {
          if (filled == lines_allocated)
            {
              lines_allocated *= 2;
              lines = (struct line_data *) xrealloc (lines, lines_allocated * sizeof (struct line_data));
            }
          lines[filled].start = &data[i];
          lines[filled].add_entries_before = 0;
          lines[filled].add_sections_before = 0;
          lines[filled].delete = 0;
          if (filled > 0)
            lines[filled - 1].size
              = lines[filled].start - lines[filled - 1].start - 1;
          filled++;
        }
      lineflag = (data[i] == '\n');
    }
  if (filled > 0)
    lines[filled - 1].size = &data[i] - lines[filled - 1].start - lineflag;

  /* Do not leave garbage in the last element.  */
  lines[filled].start = NULL;
  lines[filled].add_entries_before = NULL;
  lines[filled].add_sections_before = 0;
  lines[filled].delete = 0;
  lines[filled].size = 0;

  *nlinesp = filled;
  return lines;
}

/* Compare the menu item names in LINE1 (line length LEN1)
   and LINE2 (line length LEN2).  Return 1 if the item name
   in LINE1 is less, 0 otherwise.  */

int
menu_line_lessp (line1, len1, line2, len2)
     char *line1;
     int len1;
     char *line2;
     int len2;
{
  int minlen = (len1 < len2 ? len1 : len2);
  int i;
  
  for (i = 0; i < minlen; i++)
    {
      /* If one item name is a prefix of the other,
         the former one is less.  */
      if (line1[i] == ':' && line2[i] != ':')
        return 1;
      if (line2[i] == ':' && line1[i] != ':')
        return 0;
      /* If they both continue and differ, one is less.  */
      if (line1[i] < line2[i])
        return 1;
      if (line1[i] > line2[i])
        return 0;
    }
  /* With a properly formatted dir file,
     we can only get here if the item names are equal.  */
  return 0;
}

/* Compare the menu item names in LINE1 (line length LEN1)
   and LINE2 (line length LEN2).  Return 1 if the item names are equal,
   0 otherwise.  */

int
menu_line_equal (line1, len1, line2, len2)
     char *line1;
     int len1;
     char *line2;
     int len2;
{
  int minlen = (len1 < len2 ? len1 : len2);
  int i;
  
  for (i = 0; i < minlen; i++)
    {
      /* If both item names end here, they are equal.  */
      if (line1[i] == ':' && line2[i] == ':')
        return 1;
      /* If they both continue and differ, one is less.  */
      if (line1[i] != line2[i])
        return 0;
    }
  /* With a properly formatted dir file,
     we can only get here if the item names are equal.  */
  return 1;
}

/* This is the comparison function for qsort
   for a vector of pointers to struct spec_section.
   Compare the section names.  */

int
compare_section_names (sec1, sec2)
     struct spec_section **sec1, **sec2;
{
  char *name1 = (*sec1)->name;
  char *name2 = (*sec2)->name;
  return strcmp (name1, name2);
}

/* Insert ENTRY into the add_entries_before vector
   for line number LINE_NUMBER of the dir file.
   DIR_LINES and N_ENTRIES carry information from like-named variables
   in main.  */

void
insert_entry_here (entry, line_number, dir_lines, n_entries)
     struct spec_entry *entry;
     int line_number;
     struct line_data *dir_lines;
     int n_entries;
{
  int i;

  if (dir_lines[line_number].add_entries_before == 0)
    {
      dir_lines[line_number].add_entries_before
        = (struct spec_entry **) xmalloc (n_entries * sizeof (struct spec_entry *));
      for (i = 0; i < n_entries; i++)
        dir_lines[line_number].add_entries_before[i] = 0;
    }

  for (i = 0; i < n_entries; i++)
    if (dir_lines[line_number].add_entries_before[i] == 0)
      break;

  if (i == n_entries)
    abort ();

  dir_lines[line_number].add_entries_before[i] = entry;
}
