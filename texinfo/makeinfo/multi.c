/* multi.c -- multitable stuff for makeinfo.
   $Id: multi.c,v 1.2 1998/03/24 18:07:57 law Exp $

   Copyright (C) 1996, 97 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "system.h"
#include "makeinfo.h"

#define MAXCOLS 100             /* remove this limit later @@ */


/*
 * Output environments.  This is a hack grafted onto existing
 * structure.  The "output environment" used to consist of the
 * global variables `output_paragraph', `fill_column', etc.
 * Routines like add_char would manipulate these variables.
 *
 * Now, when formatting a multitable, we maintain separate environments
 * for each column.  That way we can build up the columns separately
 * and write them all out at once.  The "current" output environment"
 * is still kept in those global variables, so that the old output
 * routines don't have to change.  But we provide routines to save
 * and restore these variables in an "environment table".  The
 * `select_output_environment' function switches from one output
 * environment to another.
 *
 * Environment #0 (i.e., element #0 of the table) is the regular
 * environment that is used when we're not formatting a multitable.
 *
 * Environment #N (where N = 1,2,3,...) is the env. for column #N of
 * the table, when a multitable is active.
 */

/* contents of an output environment */
/* some more vars may end up being needed here later @@ */
struct env
{
  unsigned char *output_paragraph;
  int output_paragraph_offset;
  int output_column;
  int paragraph_is_open;
  int current_indent;
  int fill_column;
} envs[MAXCOLS];                /* the environment table */

/* index in environment table of currently selected environment */
static int current_env_no;

/* column number of last column in current multitable */
static int last_column;

/* flags indicating whether horizontal and vertical separators need
   to be drawn, separating rows and columns in the current multitable. */
static int hsep, vsep;

/* Output a row.  Have to keep `output_position' up-to-date for each
   character we output, or the tags table will be off, leading to
   chopped-off output files and undefined nodes (because they're in the
   wrong file, etc.).  Perhaps it would be better to accumulate this
   value somewhere and add it once at the end of the table, or return it
   as the value, but this seems simplest.  */
static void
out_char (ch)
    int ch;
{
  extern int output_position;
  putc (ch, output_stream);
  output_position++;
}


void
draw_horizontal_separator ()
{
  int i, j, s;

  for (s = 0; s < envs[0].current_indent; s++)
    out_char (' ');
  if (vsep)
    out_char ('+');
  for (i = 1; i <= last_column; i++) {
    for (j = 0; j <= envs[i].fill_column; j++)
      out_char ('-');
    if (vsep)
      out_char ('+');
  }
  out_char ('\n');
}

void
do_multitable ()
{
  int ncolumns;

  /*
   *  multitable strategy:
   *  for each item {
   *     for each column in an item {
   *      initialize a new paragraph
   *      do ordinary formatting into the new paragraph
   *      save the paragraph away
   *      repeat if there are more paragraphs in the column
   *    }
   *    dump out the saved paragraphs and free the storage
   *  }
   */

  if (multitable_active)
    {
      line_error ("Multitables cannot be nested");
      return;
    }

  /* scan the current item function to get the field widths
     and number of columns, and set up the output environment list
     accordingly. */
  ncolumns = setup_multitable_parameters ();
  if (hsep)
    draw_horizontal_separator ();

  /* The next @item command will direct stdout into the first column
     and start processing.  @tab will then switch to the next column,
     and @item will flush out the saved output and return to the first
     column.  Environment #1 is the first column.  (Environment #0 is
     the normal output) */

  ++multitable_active;
}

/* Read the parameters for a multitable from the current command
   line, save the parameters away, and return the
   number of columns. */
int
setup_multitable_parameters ()
{
  char *params = insertion_stack->item_function;
  int nchars;
  float columnfrac;
  char command[200]; /* naughty, should be no fixed limits */
  int i = 1;

  /* We implement @hsep and @vsep even though TeX doesn't.
     We don't get mixing of @columnfractions and templates right,
     but TeX doesn't either.  */
  hsep = vsep = 0;

  while (*params) {
    while (whitespace (*params))
      params++;

    if (*params == '@') {
      sscanf (params, "%200s", command);
      nchars = strlen (command);
      params += nchars;
      if (strcmp (command, "@hsep") == 0)
        hsep++;
      else if (strcmp (command, "@vsep") == 0)
        vsep++;
      else if (strcmp (command, "@columnfractions") == 0) {
        /* Clobber old environments and create new ones, starting at #1.
           Environment #0 is the normal output, so don't mess with it. */
        for ( ; i <= MAXCOLS; i++) {
          if (sscanf (params, "%f", &columnfrac) < 1)
            goto done;
          /* Unfortunately, can't use %n since some m68k-hp-bsd libc
             doesn't support it.  So skip whitespace (preceding the
             number) and then non-whitespace (the number).  */
          while (*params && (*params == ' ' || *params == '\t'))
            params++;
          /* Hmm, but what about @columnfractions 3foo?  Well, I suppose
             it's invalid input anyway.  */
          while (*params && *params != ' ' && *params != '\t'
                 && *params != '\n' && *params != '@')
            params++;
          setup_output_environment (i,
                     (int) (columnfrac * (fill_column - current_indent) + .5));
        }
      }

    } else if (*params == '{') {
      char *start = params;
      while ((*params != '}' || params[-1] == '@') && *params) {
        params++;
      }
      /* This gives us two spaces between columns.  Seems reasonable.
         Really should expand the text, though, so a template of
         `@code{foo}' has a width of five, not ten.  Also have to match
         braces, then.  How to take into account current_indent here?  */
      setup_output_environment (i++, params++ - start);
      
    } else {
      warning (_("ignoring stray text `%s' after @multitable"), params);
      break;
    }
  }

done:
  flush_output ();
  inhibit_output_flushing ();

  last_column = i - 1;
  return last_column;
}

/* Initialize environment number ENV_NO, of width WIDTH.
   The idea is that we're going to use one environment for each column of
   a multitable, so we can build them up separately and print them
   all out at the end. */
int
setup_output_environment (env_no, width)
    int env_no;
    int width;
{
  int old_env = select_output_environment (env_no);

  /* clobber old environment and set width of new one */
  init_paragraph ();

  /* make our change */
  fill_column = width;

  /* Save new environment and restore previous one. */
  select_output_environment (old_env);

  return env_no;
}

/* Direct current output to environment number N.  Used when
   switching work from one column of a multitable to the next.
   Returns previous environment number. */
int 
select_output_environment (n)
    int n;
{
  struct env *e = &envs[current_env_no];
  int old_env_no = current_env_no;

  /* stash current env info from global vars into the old environment */
  e->output_paragraph = output_paragraph;
  e->output_paragraph_offset = output_paragraph_offset;
  e->output_column = output_column;
  e->paragraph_is_open = paragraph_is_open;
  e->current_indent = current_indent;
  e->fill_column = fill_column;

  /* now copy new environment into global vars */
  current_env_no = n;
  e = &envs[current_env_no];
  output_paragraph = e->output_paragraph;
  output_paragraph_offset = e->output_paragraph_offset;
  output_column = e->output_column;
  paragraph_is_open = e->paragraph_is_open;
  current_indent = e->current_indent;
  fill_column = e->fill_column;
  return old_env_no;
}

/* advance to the next environment number */
void
nselect_next_environment ()
{
  if (current_env_no >= last_column) {
    line_error (_("Too many columns in multitable item (max %d)"), last_column);
    return;
  }
  select_output_environment (current_env_no + 1);
}


static void output_multitable_row ();

/* do anything needed at the beginning of processing a
   multitable column. */
void
init_column ()
{
  /* don't indent 1st paragraph in the item */
  cm_noindent ();

  /* throw away possible whitespace after @item or @tab command */
  skip_whitespace ();
}

/* start a new item (row) of a multitable */
int
multitable_item ()
{
  if (!multitable_active) {
    /* impossible, I think. */
    error (_("multitable item not in active multitable"));
    exit (1);
  }
  if (current_env_no > 0) {
    output_multitable_row ();
  }
  /* start at column 1 */
  select_output_environment (1);
  if (!output_paragraph) {
    line_error (_("Cannot select column #%d in multitable"), current_env_no);
    exit (FATAL);
  }

  init_column ();

  return 0;
}

static void
output_multitable_row ()
{
  int i, j, s, remaining;

  /* offset in the output paragraph of the next char needing
     to be output for that column. */
  int offset[MAXCOLS];

  for (i = 0; i <= last_column; i++)
    offset[i] = 0;

  /* select the current environment, to make sure the env variables
     get updated */
  select_output_environment (current_env_no);

#define CHAR_ADDR(n) (offset[i] + (n))
#define CHAR_AT(n) (envs[i].output_paragraph[CHAR_ADDR(n)])

  /* remove trailing whitespace from each column */
  for (i = 1; i <= last_column; i++) {
    while (cr_or_whitespace (CHAR_AT (envs[i].output_paragraph_offset - 1))) {
      envs[i].output_paragraph_offset--;
    }
  }

  /* read the current line from each column, outputting them all
     pasted together.  Do this til all lines are output from all
     columns.  */
  for (;;) {
    remaining = 0;
    /* first, see if there is any work to do */
    for (i = 1; i <= last_column; i++) {
      if (CHAR_ADDR (0) < envs[i].output_paragraph_offset) {
        remaining = 1;
        break;
      }
    }
    if (!remaining)
      break;
    
    for (s = 0; s < envs[0].current_indent; s++)
      out_char (' ');
    
    if (vsep)
      out_char ('|');

    for (i = 1; i <= last_column; i++) {
      for (s = 0; i < envs[i].current_indent; s++)
        out_char (' ');
      for (j = 0; CHAR_ADDR (j) < envs[i].output_paragraph_offset; j++) {
        if (CHAR_AT (j) == '\n')
          break;
        out_char (CHAR_AT (j));
      }
      offset[i] += j + 1;       /* skip last text plus skip the newline */
      for (; j <= envs[i].fill_column; j++)
        out_char (' ');
      if (vsep)
        out_char ('|'); /* draw column separator */
    }
    out_char ('\n');    /* end of line */
  }

  if (hsep)
    draw_horizontal_separator ();

  /* Now dispose of the buffered output. */
  for (i = 1; i <= last_column; i++) {
    select_output_environment (i);
    init_paragraph ();
  }
}

#undef CHAR_AT
#undef CHAR_ADDR

/* select a new column in current row of multitable */
void
cm_tab ()
{
  if (!multitable_active)
    error (_("ignoring @tab outside of multitable"));
  
  nselect_next_environment ();
  init_column ();
}

/* close a multitable, flushing its output and resetting
   whatever needs resetting */
void
end_multitable ()
{
  output_multitable_row ();

  /* Multitables cannot be nested.  Otherwise, we'd have to save the
     previous output environment number on a stack somewhere, and then
     restore to that environment.  */
  select_output_environment (0);
  close_paragraph ();
  insert ('\n'); /* we swallow newlines, so insert one of our own */
  
  multitable_active = 0;
  uninhibit_output_flushing ();

#if 0
  printf (_("** Multicolumn output from last row:\n"));
  for (i = 1; i <= last_column; i++) {
    select_output_environment (i);
    printf (_("* column #%d: output = %s\n"), i, output_paragraph);
  }
#endif
}
