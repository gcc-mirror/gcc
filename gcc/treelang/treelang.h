/* 

    TREELANG Compiler common definitions (treelang.h)

    Copyright (C) 1986, 87, 89, 92-96, 1997, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

/* Parse structure type.  */
enum category_enum 
{ /* These values less likely to be there by chance unlike 0/1,
      make checks more meaningful */
  token_category = 111,
  production_category = 222
};

/* Input file name and FILE.  */
extern unsigned char* in_fname;
extern FILE* yyin;

#if 0
extern int errorcount; /* In toplev.c.  */
#endif

struct token
{
  enum category_enum category; /* Token or production. */
  unsigned int type; /* Token type.  */
  /* Prior to this point, production must match token.  */
  unsigned int lineno;
  unsigned int charno;
  unsigned int length; /* The value.  */
  unsigned char* chars;
};

struct production
{
  enum category_enum category; /* Token or Production. */
  unsigned int type; /* Production type - a fake token name.  */
  /* Prior to this point, production must match token.  */
  struct token* main_token; /* Main token for error msgs; variable name token.  */

  unsigned int info[2]; /* Extra information.  */
#define NESTING_LEVEL(a) a->info[0]  /* Level used for variable definitions.  */
#define NUMERIC_TYPE(a)  a->info[1]  /* Numeric type used in type definitions and expressions.  */


#define SUB_COUNT 5
  void *sub[SUB_COUNT]; /* Sub productions or tokens.  */

#define SYMBOL_TABLE_NAME(a) (a->sub[0]) /* Name token.  */

#define EXPRESSION_TYPE(a) (a->sub[1]) /* Type identifier.  */

#define OP1(a) (a->sub[2]) /* Exp operand1.  */
#define PARAMETERS(a) (a->sub[2]) /* Function parameters.  */
#define VARIABLE(a) (a->sub[2]) /* Parameter variable ptr.  */
#define VAR_INIT(a) (a->sub[2]) /* Variable init.  */

#define OP2(a) (a->sub[3]) /* Exp operand2.  */
#define FIRST_PARMS(a) (a->sub[3]) /* Function parameters linked via struct tree_parameter_list.  */

#define OP3(a) (a->sub[4]) /* Exp operand3.  */
#define STORAGE_CLASS_TOKEN(a) (a->sub[4]) /* Storage class token.  */

  void *code; /* Back end hook for this item.  */
  struct production *next; /* Next in chains of various types.  */

  unsigned int flag1:2;
#define STORAGE_CLASS(a) a->flag1 /* Values in treetree.h.  */

  unsigned int flag2:1;
  unsigned int flag3:1;
  unsigned int flag4:1;
  unsigned int flag5:1;
  unsigned int flag6:1;
  unsigned int flag7:1;

};

/* For parser. Alternatively you can define it using %union (bison) or
   union. */
#define YYSTYPE void *

void *my_malloc (size_t size);
int insert_tree_name (struct production *prod);
struct production *lookup_tree_name (struct production *prod);
struct production *make_production (int type, struct token* main_tok);
void mark_production_used (struct production * pp);
void mark_token_used (struct token* tt);
void treelang_debug (void);

