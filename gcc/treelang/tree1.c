  /* 

    TREELANG Compiler almost main (tree1)
    Called by GCC's toplev.c

    Copyright (C) 1986, 87, 89, 92-96, 1997, 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.

    In other words, you are welcome to use, share and improve this program.
    You are forbidden to forbid anyone else to use, share and improve
    what you give them.   Help stamp out software-hoarding!  

    ---------------------------------------------------------------------------

    Written by Tim Josling 1999, 2000, 2001, based in part on other
    parts of the GCC compiler.

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "toplev.h"

#include "ggc.h"
#include "tree.h"
#include "diagnostic.h"

#include "treelang.h"
#include "treetree.h"

extern int yyparse (void);

/* Linked list of symbols - all must be unique in treelang.  */

static GTY(()) struct prod_token_parm_item *symbol_table = NULL;

/* Language for usage for messages.  */

const char *const language_string = "TREELANG - sample front end for GCC ";

/* Local prototypes.  */

void version (void);

/* Global variables.  */

extern struct cbl_tree_struct_parse_tree_top* parse_tree_top;

/* 
   Options. 
*/

/* Trace the parser.  */
unsigned int option_parser_trace = 0;

/* Trace the lexical analysis.  */

unsigned int option_lexer_trace = 0;

/* Warning levels.  */

/* Local variables.  */

const char *in_fname;	/* Input file name.  */

/* This is 1 if we have output the version string.  */

static int version_done = 0;

/* Variable nesting level.  */

static unsigned int work_nesting_level = 0;

/* Process one switch - called by toplev.c.  */

int
treelang_decode_option (num_options_left, first_option_left)
     int num_options_left ATTRIBUTE_UNUSED; 
     char** first_option_left;
{
  
  /*
    Process options - bear in mind I may get options that are really
    meant for someone else (eg the main compiler) so I have to be very
    permissive. 
    
  */
  
  if (first_option_left[0][0] != '-')
    return 0; 
  
  switch (first_option_left[0][1]) 
    {
    case '-':
      if (!strcmp (first_option_left[0],"--help"))
        {
          if (!version_done)
            {
              fputs (language_string, stdout);
              fputs (version_string, stdout);
              fputs ("\n", stdout);
              version_done = 1;
            }
          fprintf (stdout, "Usage: tree1 [switches] -o output input\n");
          return 1;
        }
    case 'v':
      if (!strcmp (first_option_left[0],"-v"))
        {
          if (!version_done)
            {
              fputs (language_string, stdout);
              fputs (version_string, stdout);
              fputs ("\n", stdout);
              version_done = 1;
            }
          return 1;
        }
    case 'y':
      if (!strcmp (first_option_left[0],"-y"))
        {
          option_lexer_trace = 1;
          option_parser_trace = 1;
          return 1;
        }
    case 'f':
      if (!strcmp (first_option_left[0],"-fparser-trace"))
        {
          option_parser_trace = 1;
          return 1;
        }
      if (!strcmp (first_option_left[0],"-flexer-trace"))
        {
          option_lexer_trace = 1;
          return 1;
        }
      return 0;

    case 'w':
      if (!strcmp (first_option_left[0],"-w"))
        {
          /* Tolerate this option but ignore it - we always put out
             all warnings.  */
          return 1;
        }
      return 0;

    case 'W':
      if (!strcmp (first_option_left[0],"-Wall"))
        {
          return 1;
        }
      return 0;

    default:
      return 0;
    }

  return 0;

}

/* Language dependent parser setup.  */

bool
treelang_init ()
{
  in_fname = main_input_filename;

  /* Set up the declarations needed for this front end.  */
  lineno = 0;

  /* Init decls etc.  */

  treelang_init_decl_processing ();

  /* This error will not happen from GCC as it will always create a
     fake input file.  */
  if (!in_fname || in_fname[0] == ' ' || !in_fname[0]) 
    {
      if (!version_done)
        {
          fprintf (stderr, "No input file specified, try --help for help\n");
          exit (1);
        }

      return false;
    }

  yyin = fopen (in_fname, "r");
  if (!yyin)
    {
      fprintf (stderr, "Unable to open input file %s\n", in_fname);
      exit (1);
    }

  return true;
}

/* Language dependent wrapup.  */

void 
treelang_finish (void)
{
  fclose (yyin);
}

/* Parse a file.  Debug flag doesn't seem to work. */

void
treelang_parse_file (int debug_flag ATTRIBUTE_UNUSED)
{
  treelang_debug ();
  yyparse ();
}

/* Allocate SIZE bytes and clear them.  Not to be used for strings
   which must go in stringpool.  */

void *
my_malloc (size_t size)
{
  void *mem;
  mem = ggc_alloc (size);
  if (!mem)
    {
      fprintf (stderr, "\nOut of memory\n");
      abort ();
    }
  memset (mem, 0, size);
  return mem;
}

/* Look up a name in PROD->SYMBOL_TABLE_NAME in the symbol table;
   return the symbol table entry from the symbol table if found there,
   else 0.  */

struct prod_token_parm_item*
lookup_tree_name (struct prod_token_parm_item *prod)
{
  struct prod_token_parm_item *this;
  struct prod_token_parm_item *this_tok;
  struct prod_token_parm_item *tok;

  sanity_check (prod);
  
  tok = SYMBOL_TABLE_NAME (prod);
  sanity_check (tok);
  
  for (this = symbol_table; this; this = this->tp.pro.next)
    {
      sanity_check (this);
      this_tok = this->tp.pro.main_token;
      sanity_check (this_tok);
      if (tok->tp.tok.length != this_tok->tp.tok.length) 
        continue;
      if (memcmp (tok->tp.tok.chars, this_tok->tp.tok.chars, this_tok->tp.tok.length))
        continue;
      if (option_parser_trace)
        fprintf (stderr, "Found symbol %s (%i:%i) as %i \n", tok->tp.tok.chars, 
                tok->tp.tok.lineno, tok->tp.tok.charno, NUMERIC_TYPE (this));
      return this;
    }
  if (option_parser_trace)
    fprintf (stderr, "Not found symbol %s (%i:%i) as %i \n", tok->tp.tok.chars, 
            tok->tp.tok.lineno, tok->tp.tok.charno, tok->type);
  return NULL;
}

/* Insert name PROD into the symbol table.  Return 1 if duplicate, 0 if OK.  */

int
insert_tree_name (struct prod_token_parm_item *prod)
{
  struct prod_token_parm_item *tok;
  tok = SYMBOL_TABLE_NAME (prod);
  sanity_check (prod);
  if (lookup_tree_name (prod))
    {
      fprintf (stderr, "%s:%i:%i duplicate name %s\n", in_fname, tok->tp.tok.lineno, 
               tok->tp.tok.charno, tok->tp.tok.chars);
      errorcount++;
      return 1;
    }
  prod->tp.pro.next = symbol_table;
  NESTING_LEVEL (prod) = work_nesting_level;
  symbol_table = prod;
  return 0;
}

/* Create a struct productions of type TYPE, main token MAIN_TOK.  */

struct prod_token_parm_item *
make_production (int type, struct prod_token_parm_item *main_tok)
{
  struct prod_token_parm_item *prod;
  prod = my_malloc (sizeof (struct prod_token_parm_item));
  prod->category = production_category;
  prod->type = type;
  prod->tp.pro.main_token = main_tok;
  return prod;
} 

/* Abort if ITEM is not a valid structure, based on 'category'.  */

void
sanity_check (struct prod_token_parm_item *item)
{
  switch (item->category)
    {
    case   token_category:
    case production_category:
    case parameter_category:
      break;
      
    default:
      abort ();
    }
}  

/* New garbage collection regime see gty.texi.  */
#include "gt-treelang-tree1.h"
/*#include "gt-treelang-treelang.h"*/
#include "gtype-treelang.h"
