/* Implement Input/Output runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser, et al

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>
#if _TEXTIO_DEBUG_
#include <stdio.h>
#endif

#include "bitstring.h"
#include "auxtypes.h"
#include "iomodes.h"
#include "format.h"
#include "fileio.h"
#include "ioerror.h"

#define CH_BYTE_MIN   0xffffff80L
#define CH_BYTE_MAX   0x0000007fL
#define CH_UBYTE_MAX  0x000000ffUL
#define CH_INT_MIN    0xffff8000L
#define CH_INT_MAX    0x00007fffL
#define CH_UINT_MAX   0x0000ffffUL
#define CH_LONG_MIN   0x80000000L
#define CH_LONG_MAX   0x7fffffffL
#define CH_ULONG_MAX  0xffffffffUL

#ifndef M_LN2
#define M_LN2   0.69314718055994530942
#endif
#ifndef M_LN10
#define M_LN10          2.30258509299404568402
#endif

#define DMANTDIGS  (1 + (int)(DBL_MANT_DIG * M_LN2 / M_LN10))
#define FMANTDIGS  (1 + (int)(FLT_MANT_DIG * M_LN2 / M_LN10))

/* float register length */
#define MAXPREC 40

#define LET 0x0001
#define BIN 0x0002
#define DEC 0x0004
#define OCT 0x0008
#define HEX 0x0010
#define USC 0x0020
#define BIL 0x0040
#define SPC 0x0080
#define SCS 0x0100
#define IOC 0x0200
#define EDC 0x0400
#define CVC 0x0800

#define isDEC(c)  ( chartab[(c)] & DEC )
#define isCVC(c)  ( chartab[(c)] & CVC )
#define isEDC(c)  ( chartab[(c)] & EDC )
#define isIOC(c)  ( chartab[(c)] & IOC )
#define isUSC(c)
#define isXXX(c,XXX)  ( chartab[(c)] & XXX )

/*
 *  local definitions
 */

static
short int chartab[256] = {
  0, 0, 0, 0, 0, 0, 0, 0, 
  0, SPC, SPC, SPC, SPC, SPC, 0, 0, 

  0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 

  SPC, IOC, 0, 0, 0, 0, 0, 0, 
  SCS, SCS, SCS, SCS+IOC, SCS, SCS+IOC, SCS, SCS+IOC, 
  BIN+OCT+DEC+HEX, BIN+OCT+DEC+HEX, OCT+DEC+HEX, OCT+DEC+HEX, OCT+DEC+HEX,
     OCT+DEC+HEX, OCT+DEC+HEX, OCT+DEC+HEX, 
  DEC+HEX, DEC+HEX, SCS, SCS, SCS+EDC, SCS+IOC, SCS+EDC, IOC, 

  0, LET+HEX+BIL, LET+HEX+BIL+CVC, LET+HEX+BIL+CVC, LET+HEX+BIL, LET+HEX, 
     LET+HEX+CVC, LET, 
  LET+BIL+CVC, LET, LET, LET, LET, LET, LET, LET+CVC, 

  LET, LET, LET, LET, LET+EDC, LET, LET, LET,
  LET+EDC, LET, LET, SCS, 0, SCS, 0, USC, 

  0, LET+HEX, LET+HEX, LET+HEX, LET+HEX, LET+HEX, LET+HEX, LET, 
  LET, LET, LET, LET, LET, LET, LET, LET, 

  LET, LET, LET, LET, LET, LET, LET, LET,
  LET, LET, LET, 0, 0, 0, 0, 0 
};

typedef enum {
  FormatText, FirstPercent, RepFact, ConvClause, EditClause, ClauseEnd,
  AfterWidth, FractWidth, FractWidthCont, ExpoWidth, ExpoWidthCont, 
  ClauseWidth, CatchPadding, LastPercent
} fcsstate_t;

#define CONVERSIONCODES "CHOBF"
typedef enum {
  DefaultConv, HexConv, OctalConv, BinaryConv, ScientConv
} convcode_t;

static
short int base[4] = { 10, 16, 8, 2 };

static
short int dset[4] = { DEC, HEX, OCT, BIN };

#define EDITCODES "X<>T"
typedef enum {
  SpaceSkip, SkipLeft, SkipRight, Tabulation
} editcode_t;

#define IOCODES "/+-?!="
typedef enum {
  NextRecord, NextPage, CurrentLine, Prompt, Emit, EndPage
} iocode_t;

typedef enum { 
  ConvAct, EditAct, IOAct
} acttype_t;

typedef enum {
  NormalEnd, EndAtParen, TextFailEnd 
} formatexit_t;

static
double ep_1[10] = {
  1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9 };
static
double ep_10[10] = {
  1e0, 1e10, 1e20, 1e30, 1e40, 1e50, 1e60, 1e70, 1e80, 1e90 };
static
double ep_100 = 1e100;

/* float register */
static
unsigned char floatdig[MAXPREC];

/*
 *  global io variables
 */

static Text_Mode*      textptr = NULL;
static VarString*      textrecptr;

static int             actual_index;
static int             maximum_index;
static int             iolist_index;

static __tmp_IO_list*  iolistptr;
static int             iolistlen;
static char*           iostrptr;
static int             iostrlen;


static convcode_t     convcode;
static editcode_t     editcode;
static iocode_t       iocode;
static unsigned long  repetition;
static Boolean        leftadjust;
static Boolean        overflowev;
static Boolean        dynamicwid;
static Boolean        paddingdef;
static char           paddingchar;
static Boolean        fractiondef;
static unsigned long  fractionwidth;
static Boolean        exponentdef;
static unsigned long  exponentwidth;
static unsigned long  clausewidth;
static signed long    textindex;
  
static
__tmp_IO_enum_table_type bool_tab[] = 
   { { 0, "FALSE" }, 
     { 1, "TRUE"  },
     { 0 , NULL   }  };

/*
 * case insensitive compare: s1 is zero delimited, s2 has n chars
 */
static
int casncmp( const char* s1, const char* s2, int n )
{
  int res = 0;
  while( n-- )
  {
    if( (res = toupper(*s1++) - toupper(*s2++)) ) 
      return res;
  }
  return *s1;
}

/*
 * skip spaces with blank equal to tab
 */
static
int skip_space( int limit )
{
  int skipped = 0;
  while( actual_index < limit &&
         (iostrptr[actual_index] == ' ' || iostrptr[actual_index] == '\t' ) )
  {
    actual_index++;
    skipped++;
  }
  return skipped;
}

/*
 * skip leading pad characters
 */
static
int skip_pad( int limit )
{
  int skipped = 0;
  while( actual_index < limit && iostrptr[actual_index] == paddingchar )
  {
    actual_index++;
    skipped++;
  }
#if _TEXTIO_DEBUG_
  printf( "skipping '%c' until %d: %d\n", paddingchar, limit, skipped );
#endif
  return skipped;
}

/*
 * backup trailing pad characters
 */
static
int piks_pad( int start, int limit )
{
  int skipped = 0;
  while( start >/***=*/ limit && iostrptr[--start] == paddingchar )
  {
    skipped++;
  }
#if _TEXTIO_DEBUG_
  printf( "piksing '%c' from %d until %d: %d\n", 
          paddingchar, start, limit, skipped );
#endif
  return skipped;
}

/*
 * parse an integer
 */
static
int parse_int( int limit, int SET, int base, 
               unsigned long* valptr, int* signptr )
{
  int           parsed = actual_index;
  Boolean       digits = False;
  unsigned long value  = 0;
  char          curr;
  int           dig;

  if( actual_index >= limit )
    IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_INT );
  *signptr = +1;
  if( iostrptr[actual_index] == '+' )
    actual_index++;
  else
    if( iostrptr[actual_index] == '-' )
    {  *signptr = -1;
       actual_index++;
    }

  for( ; actual_index < limit; actual_index++ )
  {
    curr = iostrptr[actual_index];
    if( curr == '_' ) continue;
    if( isXXX(curr,SET) )
    {
      digits = True;
      dig = curr <= '9' ? curr - '0' : toupper(curr) - 'A' + 10;
      if( value > (ULONG_MAX - dig)/base )
        IOEXCEPTION( TEXTFAIL, INT_VAL_OVERFLOW );
      value = value*base + dig;
      continue;
    }
    break;
  }
  if( !digits )
    IOEXCEPTION( TEXTFAIL, NO_DIGITS_FOR_INT );

  *valptr = value;
#if _TEXTIO_DEBUG_
  printf( "parsing for int until %d, base %d: %u\n", limit, base, value );
#endif
  return actual_index - parsed;
}

static
double
make_float( int dexp, int sign )
{
  double value = atof( floatdig );
#if _TEXTIO_DEBUG_
  printf( " value = %25.20e, dexp = %d\n", value, dexp );
#endif
  while( dexp >= 100 )
    value *= ep_100, dexp -= 100;
  if( dexp >= 10 )
    value *= ep_10[dexp/10], dexp %= 10;
  if( dexp > 0 )
    value *= ep_1[dexp];

  while( dexp <= -100 )
    value /= ep_100, dexp += 100;
  if( dexp <= -10 )
    value /= ep_10[-dexp/10], dexp %= 10;
  if( dexp < 0 )
    value /= ep_1[-dexp];

  return  sign ? -value : value;
}

/* %C -> fixed point   [+|-]<digit>+[.<digit>*]  */
static
int parse_fixedpoint( int limit, double* valptr )
{
  int           parsed = actual_index;
  Boolean       digits = False;
  int           sdig = 0;
  double        value;
  char          curr;
  int           sign = False;
  int           expo = 0;

  if( actual_index >= limit )
    IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_FLOAT );
  if( iostrptr[actual_index] == '+' )
    actual_index++;
  else
    if( iostrptr[actual_index] == '-' )
    {
       sign = True;
       actual_index++;
    }

  floatdig[0] = '.';
  for( ; actual_index < limit; actual_index++ )
  {
    curr = iostrptr[actual_index];
    if( ! isDEC(curr) )
      break;
    digits = True;
    if( sdig < MAXPREC - 1 )
    {
      if( sdig || curr != '0' )
      {
        floatdig[++sdig] = curr;
        expo++;
      }
    }
    else
      if( sdig )
        expo++;
  }
  if( digits && curr == '.' )
  { 
    actual_index++;
    for( ; actual_index < limit; actual_index++ )
    {
      curr = iostrptr[actual_index];
      if( !isDEC(curr) )
        break;
      if( sdig < MAXPREC - 1 )
      {
        if( sdig || curr != '0' )
          floatdig[++sdig] = curr;
        else
          expo--;
      }
    }
  }
  floatdig[++sdig] = '\0';

  if( !digits )
    IOEXCEPTION( TEXTFAIL, NO_DIGITS_FOR_FLOAT );

  *valptr = make_float( expo, sign);
  return actual_index - parsed;
}


typedef enum {
  s_sign, s_dig, s_period, s_fraca, s_fracb, s_expo, s_exposign, 
  s_expoa, s_expob }
scient_t;

/* %C -> scientific   [+|-]<digit>[.<digit>*]E[=|-]<digit>+  */
static
int parse_scientific( int limit, double* valptr, double dmin, double dmax )
{
  int           parsed = actual_index;
  int           sdig = 0;
  char          curr;
  double        value;
  int           sign = False;
  int           expo = 0;           
  int           expo_sign = +1;

  scient_t      state = s_sign;  

  if( actual_index >= limit )
    IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_FLOAT );

  floatdig[0] = '.';
  for( ; actual_index < limit; actual_index++ )
  {
    curr = iostrptr[actual_index];
    switch( state )
    {
    case s_sign:
      if( iostrptr[actual_index] == '+' )
      {
        state = s_dig;
        break;
      }
      if( iostrptr[actual_index] == '-' )
      {
        sign = True;
        state = s_dig;
        break;
      }
      /* fall through - no break */
    case s_dig:
      if( isDEC(curr) && curr > '0' )
      {
        floatdig[++sdig] = curr;
        state = s_period;
        break;
      }
      IOEXCEPTION( TEXTFAIL, NO_DIGITS_FOR_FLOAT );
    case s_period:
      if( curr == '.' )
      {
        state = s_fraca;
        break;
      }
      if( curr == 'E' )
      {
        state = s_exposign;
        break;
      }
      IOEXCEPTION( TEXTFAIL, NO_EXPONENT );
    case s_fraca:
      if( isDEC(curr) )
      {
        floatdig[++sdig] = curr;
        state = s_fracb;
        break;
      }
      IOEXCEPTION( TEXTFAIL, NO_DIGITS_FOR_FLOAT );
    case s_fracb:
      if( isDEC(curr) )
      {
        if( sdig < MAXPREC - 1 )
          floatdig[++sdig] = curr;
        break;
      }
      if( curr == 'E' )
      {
        state = s_exposign;
        break;
      }
      IOEXCEPTION( TEXTFAIL, NO_EXPONENT );
    case s_exposign:
      if( iostrptr[actual_index] == '+' )
      {
        state = s_expoa;
        break;
      }
      if( iostrptr[actual_index] == '-' )
      {
        expo_sign = -1;
        state = s_expoa;
        break;
      }
    case s_expoa:
      if( isDEC(curr) )
      {
        expo = curr - '0';
        state = s_expob;
        break;
      }
      IOEXCEPTION( TEXTFAIL, NO_EXPONENT );
    case s_expob:
      expo = expo*10 + (curr - '0');
      if( expo > 1000 )
        IOEXCEPTION( TEXTFAIL, REAL_OVERFLOW );
    }
  }
  if( state != s_expob ) 
    IOEXCEPTION( TEXTFAIL, NO_EXPONENT );

  expo *= expo_sign;
  expo++;

  floatdig[++sdig] = '\0';

  *valptr = make_float( expo, sign );
  return actual_index - parsed;
}


static
int parse_set( int limit, __tmp_IO_enum_table_type* tabptr, 
               unsigned long* valptr )
{
  int    parsed = actual_index;
  char   curr;
  __tmp_IO_enum_table_type* etptr;

  if( actual_index >= limit )
    IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_SET );

  curr = iostrptr[actual_index];
  if( isXXX(curr,LET+USC) )
    actual_index++;
  else
    IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_SET );

  for( ; actual_index < limit; actual_index++ )
  {    
    if( ! isXXX(iostrptr[actual_index],LET+DEC+USC) )
      break;
  }

  if( tabptr )
     while( tabptr->name )
     {
       if( !casncmp( tabptr->name, &iostrptr[parsed], actual_index-parsed ) )
       {
         *valptr = tabptr->value;
#if _TEXTIO_DEBUG_
         printf( "parsing set value until %d: %u\n", limit, tabptr->value );
#endif
         return actual_index - parsed;         
       }
       tabptr++;
     }
  IOEXCEPTION( TEXTFAIL, SET_CONVERSION_ERROR ); 
}

static
int parse_bit( int limit, char* bitptr )
{
  int parsed = actual_index;
  int i = 0;
  char curr;

  if( actual_index >= limit )
    IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_BOOLS );

  for( ; actual_index < limit; actual_index++ )
  {
    curr = iostrptr[actual_index] - '0';
    if( curr == 0 || curr == 1 )
      /* __setbitinset( i++, bitptr, limit, curr ); */
      __setbitpowerset (bitptr, limit, 0, i++, curr, __FILE__, __LINE__);
    else
      break;
  }
  return actual_index - parsed;
}

static
char* myultoa( unsigned long ul, char* buf, int base )
{
  char*         res = buf;
  unsigned long h = ul/base;
  unsigned long q = 1;

  while( h >= q ) q *= base;
  while( q > 0 )
  {
    *buf++ = "0123456789ABCDEF"[ul/q];
    ul %= q;
    q /= base;
  }
  *buf++ = '\0';
  return res;
}

/*
 *  convert a bit string from src, bit offset up to len
 */
static
char* bitput( char* dst, char* src, int offset, int len )
{
  char* res = dst;
  int i;
  for( i = offset; i < len; i++ )
  {
    *dst++ = __inpowerset( i, src, len, 0 ) ? '1' : '0';
  }
  return res;
}

/*
 * dround: round decimal register *digptr starting at digit mdigs,
 *         on carry advance begin of digit sequence and bump exponent
 */ 
static
char*
dround( char* digptr, int mdigs, int* deptr )
{
  int carry;
#if _TEXTIO_DEBUG_
  printf( "Rounding from %d\n", mdigs );
#endif
  if( digptr[mdigs] >= 5 )
  {
    carry = 1;
    while( carry )
    {
      digptr[--mdigs]++;
      if( digptr[mdigs] >= 10 )
        digptr[mdigs] = 0;
      else
        carry = 0;
    }
  }
  if( mdigs < 0 )
  {
    digptr[--mdigs] = 1;
    (*deptr)++;
    return digptr - 1;
  }
  else
    return digptr;
}

/*
 * mydtoa: convert val with a precision of mantdigs to a decimal fraction
 *         first digit is at **fstdiptr, decimal exponent is at *deptr
 */
static
char*
mydtoa( double val, int mantdigs, int* deptr, int* sgnptr )
{
  double m;
  int be;
  int de = -1;
  int fstdig = 0;
  int idig; 
  char* digptr = floatdig+2;

  floatdig[0] = floatdig[1] = 0;

  if( val < 0 ) 
    *sgnptr = -1, val = fabs( val );
  else
    *sgnptr = +1;

  /* split the value */
  m = frexp( val, &be ) * 10.0;

  /* 5.0 <= m < 10.0 */
  while( be > 0 )
  {
    de++; be--; m /= 5.0;
    if( m < 1.0 )
      m *= 10.0, de--;
  }
  while( be < 0 )
  {
    de--; be++; m *= 5.0;
    if( m >= 10.0 )
      m /= 10.0, de++;
  }

  for( idig = 0; idig < mantdigs; idig++ )
  {
    digptr[idig] = (int)m;
    m = (m - digptr[idig])*10.0;
  }
  digptr[idig] = (int)m;

  *deptr = de;
  return dround( digptr, mantdigs, deptr );
}

#define PUT(c) \
  { if( ifst <= ++iprt && iprt <= ilst ) *dst++ = c; }

static
char*
fixput( char* dst, char* src, 
        int ifst, int ilst, 
        int sign, int fst, int lst, 
        int nid, int nfd )
{
  char* dstsav = dst;
  int idig;
  int iprt = 0;

  if( sign < 0 )
    PUT( '-' );
  for( idig = nid; idig >= -nfd; idig-- )
  {
    if (idig == -1)
      PUT( '.' );
    PUT( idig > fst || lst >= idig ? '0': '0' + *src++ );
  }
  return dstsav;
}

static
char*
sciput( char* dst, char* src, char* expbeg,
        int ifst, int ilst, 
        int sign, int de, int expwid )
{
  char* dstsav = dst;
  int iprt = 0;
  int nfd = fractionwidth;
  int explen = strlen( expbeg );

  if( sign < 0 )
    PUT( '-' );
  PUT( '0' + *src++ );
  PUT( '.' );

  while( nfd-- )
    PUT( '0' + *src++ );
  PUT( 'E' );
  PUT( de >= 0 ? '+' : '-' );
  while( expwid > explen )
  {
    PUT( '0' );
    expwid--;
  }
  while( explen-- )
    PUT( *expbeg++ );
  return dstsav;
}

/*
 *  handle dynamic field width
 */ 
static
get_field_width( void )
{
  unsigned long  width;
  unsigned long  ulongval;
           long  longval;
  __tmp_IO_list  io;
   

  if( ++iolist_index > iolistlen )
    IOEXCEPTION( TEXTFAIL, IOLIST_EXHAUSTED );  

  io = *iolistptr++;

  /* must be integer, >= 0 */
  switch( io.__descr )
  {
  case __IO_ByteVal:
    longval = io.__t.__valbyte; 
    goto signed_fieldwidth;
  case __IO_UByteVal:
    width = io.__t.__valubyte; 
    goto unsigned_fieldwidth;
  case __IO_IntVal:
    longval = io.__t.__valint; 
    goto signed_fieldwidth;
  case __IO_UIntVal:
    width = io.__t.__valuint; 
    goto unsigned_fieldwidth;
  case __IO_LongVal:
    longval = io.__t.__vallong; 
    goto signed_fieldwidth;
  case __IO_ULongVal:
    width = io.__t.__valulong; 
    goto unsigned_fieldwidth;
  case __IO_ByteLoc:
    longval = *(signed char*)io.__t.__locint; 
    goto signed_fieldwidth;
  case __IO_UByteLoc:
    width = *(unsigned char*)io.__t.__locint; 
    goto unsigned_fieldwidth;
  case __IO_IntLoc:
    longval = *(signed short*)io.__t.__locint; 
    goto signed_fieldwidth;
  case __IO_UIntLoc:
    width = *(unsigned short*)io.__t.__locint; 
    goto unsigned_fieldwidth;
  case __IO_LongLoc:
    longval = *(signed long*) io.__t.__locint; 
    goto signed_fieldwidth;
  case __IO_ULongLoc:
    width = *(unsigned long*)io.__t.__locint; 
    goto unsigned_fieldwidth;
  default:
    IOEXCEPTION( TEXTFAIL, NON_INT_FIELD_WIDTH );
  }

signed_fieldwidth: ;
  if( longval < 0 )
    IOEXCEPTION( TEXTFAIL, NEGATIVE_FIELD_WIDTH );
  width = longval;

unsigned_fieldwidth: ;
  return width;
}


static
void inpconv( void )
{
  __tmp_IO_list  io;
  int            width;
  int            limit;
  int            skiplim;
  int            skipped;
  int            bypass;
  int            parsed;
  Boolean        fixedchars;
  int            fixedlen;
  unsigned char  curr;
  double         dval;
  float          fval;

  __tmp_IO_long  lval;
  int            sign;
  unsigned long  umin;
  unsigned long  umax;
    signed long  smin;
    signed long  smax;
  int            ilen;
  short unsigned slen;
  __tmp_IO_enum_table_type* settabptr; 

  while( repetition-- )
  {
    if( ++iolist_index > iolistlen )
      IOEXCEPTION( TEXTFAIL, IOLIST_EXHAUSTED );  

    io = *iolistptr++;

    if( dynamicwid )
      width = get_field_width();
    else
      width = clausewidth;

    bypass = skipped = 0;
    if( width )
    {
      if( actual_index + width > iostrlen )
        IOEXCEPTION( TEXTFAIL, NOT_ENOUGH_CHARS );

      switch(io.__descr)
      {
      case __IO_CharLoc:
      case __IO_CharRangeLoc:
        fixedchars = True;
        fixedlen = 1;
        break;
      case __IO_CharStrLoc:
        fixedchars = True;
        fixedlen = io.__t.__loccharstring.string_length;
        break;
      default:
        fixedchars = False;
        break;
      }
         
      if( leftadjust )
      {
        skiplim = fixedchars ? actual_index + fixedlen
                             : actual_index;
        bypass = skipped = piks_pad( actual_index + width, skiplim );
      }
      else
      {
        skiplim = fixedchars ? actual_index + width - fixedlen
                             : actual_index + width;
        skipped = skip_pad( skiplim );
      }
      width -= skipped;
      limit = actual_index + width;
    }
    else
    { /* free format */
      if( paddingdef || !( io.__descr == __IO_CharLoc ||
                           io.__descr == __IO_CharRangeLoc || 
                           io.__descr == __IO_CharStrLoc ||
                           io.__descr == __IO_CharVaryingLoc ) )
        if( paddingchar == ' ' || paddingchar == '\t' )
          skip_space( iostrlen );
        else
          skip_pad( iostrlen );
      limit = iostrlen;
    }

    switch( io.__descr )
    {
    case __IO_ByteLoc:
      ilen = 1;
      smin = CH_BYTE_MIN;
      smax = CH_BYTE_MAX;
      goto parse_signed_int;
    case __IO_UByteLoc:
      ilen = 1;
      umin = 0;
      umax = CH_UBYTE_MAX;
      goto parse_unsigned_int;
    case __IO_IntLoc:
      ilen = 2;
      smin = CH_INT_MIN;
      smax = CH_INT_MAX;
      goto parse_signed_int;
    case __IO_UIntLoc:
      ilen = 2;
      umin = 0;
      umax = CH_UINT_MAX;
      goto parse_unsigned_int;
    case __IO_LongLoc:
      ilen = 4;
      smin = CH_LONG_MIN;
      smax = CH_LONG_MAX;
      goto parse_signed_int;
    case __IO_ULongLoc:
      ilen = 4;
      umin = 0;
      umax = CH_ULONG_MAX;
      goto parse_unsigned_int;

    case __IO_ByteRangeLoc:
      ilen = 1;
      smin = io.__t.__locintrange.lower.slong;
      smax = io.__t.__locintrange.upper.slong;
      goto parse_signed_int;
    case __IO_UByteRangeLoc:
      ilen = 1;
      umin = io.__t.__locintrange.lower.ulong;
      umax = io.__t.__locintrange.upper.ulong;
      goto parse_unsigned_int;
    case __IO_IntRangeLoc:
      ilen = 2;
      smin = io.__t.__locintrange.lower.slong;
      smax = io.__t.__locintrange.upper.slong;
      goto parse_signed_int;
    case __IO_UIntRangeLoc:
      ilen = 2;
      umin = io.__t.__locintrange.lower.ulong;
      umax = io.__t.__locintrange.upper.ulong;
      goto parse_unsigned_int;
    case __IO_LongRangeLoc:
      ilen = 4;
      smin = io.__t.__locintrange.lower.slong;
      smax = io.__t.__locintrange.upper.slong;
      goto parse_signed_int;
    case __IO_ULongRangeLoc:
      ilen = 4;
      umin = io.__t.__locintrange.lower.ulong;
      umax = io.__t.__locintrange.upper.ulong;
      goto parse_unsigned_int;

    case __IO_BoolLoc:
      ilen = 1;
      umin = 0;
      umax = 1;
      settabptr = bool_tab;
      goto parse_set;
    case __IO_BoolRangeLoc:
      ilen = 1;
      umin = io.__t.__locboolrange.lower;
      umax = io.__t.__locboolrange.upper;
      settabptr = bool_tab;
      goto parse_set;

    case __IO_SetLoc:
      ilen = io.__t.__locsetrange.length;
      settabptr = io.__t.__locsetrange.name_table;
      umin = 0;
      umax = CH_ULONG_MAX;
      goto parse_set;
    case __IO_SetRangeLoc:
      ilen = io.__t.__locsetrange.length;
      settabptr = io.__t.__locsetrange.name_table;
      umin = io.__t.__locsetrange.lower;
      umax = io.__t.__locsetrange.upper;
      goto parse_set;

    case __IO_CharLoc:
      umin = 0;
      umax = 0xff;
      goto parse_char;
    case __IO_CharRangeLoc:
      umin = io.__t.__loccharrange.lower;
      umax = io.__t.__loccharrange.upper;
      goto parse_char;

    case __IO_CharVaryingLoc:
      if( convcode != DefaultConv )
        IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
      slen = io.__t.__loccharstring.string_length;
      if( (parsed = limit - actual_index) < slen )
        slen = parsed;
      else
        parsed = slen;  
      memcpy( io.__t.__loccharstring.string + 2, 
              &iostrptr[actual_index], parsed );
      MOV2(io.__t.__loccharstring.string,&slen);
      actual_index += parsed;
      goto check_field_complete;


    case __IO_CharStrLoc:
      if( convcode != DefaultConv )
        IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
      if( actual_index + io.__t.__loccharstring.string_length > limit )
        IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_CHARS );
      memcpy( io.__t.__loccharstring.string,
              &iostrptr[actual_index],
              parsed = io.__t.__loccharstring.string_length );
      actual_index += parsed;
      goto check_field_complete;

    case __IO_BitStrLoc:
      if( convcode != DefaultConv )
        IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
      parsed = parse_bit( limit, io.__t.__loccharstring.string );
      if( parsed < io.__t.__loccharstring.string_length )
        IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_BOOLS );
      goto check_field_complete;

    case __IO_LongRealLoc:
    case __IO_RealLoc:
      switch( convcode )
      {
      case ScientConv:
        parse_scientific( limit, &dval, DBL_MIN, DBL_MAX );
        break;
      case DefaultConv:
        parse_fixedpoint( limit, &dval );
        break;
      default:
        IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
      }
      if( io.__descr == __IO_LongRealLoc )
        memcpy( io.__t.__loclongreal, &dval, sizeof(double) );
      else
      {
        fval = (float)dval;
        MOV4(io.__t.__locreal,&fval);
      }
      goto check_field_complete;
    default:
      IOEXCEPTION( TEXTFAIL, INVALID_IO_LIST );
    }


parse_signed_int: ;
    if( convcode == ScientConv )
      IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
    parsed = parse_int( limit, dset[convcode], base[convcode], 
                        &lval.ulong, &sign );
    if( sign < 0 )
    {
      if( lval.ulong > (unsigned long)CH_LONG_MIN )
        IOEXCEPTION( TEXTFAIL, INTEGER_RANGE_ERROR );
      lval.slong = -lval.ulong;
    }
    else
    {
      /* not needed: lval.slong = lval.ulong; */
      /* Hack: sign extension for bin/oct/dec if no sign present */
      if( convcode != DefaultConv && lval.ulong & (1 << (ilen*8-1)) )
      {
        if( ilen < 4 )
          lval.ulong |= 0xFFFFFFFF << ilen*8;
      }
      else
        if( lval.ulong > (unsigned long)CH_LONG_MAX )
          IOEXCEPTION( TEXTFAIL, INTEGER_RANGE_ERROR );
    }
    if( lval.slong < smin || smax < lval.slong )
      IOEXCEPTION( TEXTFAIL, INTEGER_RANGE_ERROR );
    goto store_int;

parse_unsigned_int: ;
    if( convcode == ScientConv )
      IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
    parsed = parse_int( limit, dset[convcode], base[convcode],
                        &lval.ulong, &sign );
    if( sign < 0 ||  lval.ulong < umin || umax < lval.ulong )
      IOEXCEPTION( TEXTFAIL, INTEGER_RANGE_ERROR );
    goto store_int;

parse_set: ;
    if( convcode != DefaultConv )
      IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
    parsed = parse_set( limit, settabptr, &lval.ulong );
    if( lval.ulong < umin || umax < lval.ulong )
      IOEXCEPTION( TEXTFAIL, SET_RANGE_ERROR );
    goto store_int;

store_int: ;
    switch( ilen )
    {
    case 1:
      *(unsigned char*)io.__t.__locint = lval.ulong;
      break;
    case 2:
      slen = lval.ulong;
      MOV2(io.__t.__locint,&slen);
      break;
    case 4:
      MOV4(io.__t.__locint,&lval.ulong);
      break;
    default:
      IOEXCEPTION( TEXTFAIL, INTERNAL_ERROR );
    }
    goto check_field_complete;

parse_char: ;
    if( convcode != DefaultConv )
      IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
    if( actual_index >= limit )
      IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_CHARS );
    curr = iostrptr[actual_index++];
    parsed = 1;
    if( curr < umin || umax < curr )
      IOEXCEPTION( TEXTFAIL, CHAR_RANGE_ERROR );
    *io.__t.__locchar = curr;
    goto check_field_complete;

check_field_complete: ;
    actual_index += bypass;    
    if( width > parsed )
      IOEXCEPTION( TEXTFAIL, INVALID_CHAR );
  }
}

static
void inpedit( void )
{
  int           nchars;

  if( dynamicwid ) 
    clausewidth = get_field_width();

  switch( editcode )
  { 
  case SpaceSkip:
    nchars = repetition*clausewidth;
    if( actual_index + nchars > iostrlen )
      IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_EDIT );
    for( ; nchars ; nchars-- )
      if( iostrptr[actual_index++] != ' ' )
        IOEXCEPTION( TEXTFAIL, NO_SPACE_TO_SKIP );
    break; 

  case SkipLeft:
    nchars = repetition*clausewidth;
    if( (actual_index -= nchars) < 0 )
      IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_EDIT );
    break;

  case SkipRight:
    nchars = repetition*clausewidth;
    if( (actual_index += nchars) > iostrlen )
      IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_EDIT );
    break;
  
  case Tabulation:
    if( (actual_index = clausewidth) > iostrlen )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );
    break;
  }
}

static
void outconv( void )
{
  unsigned long             width;
  char                      itembuf[33]; 
  unsigned long             ulongval;
           long             longval;
  __tmp_IO_list             io;
  __tmp_IO_enum_table_type* etptr;
  char*                     itembeg;
  unsigned long             itemlen;
  double                    doubleval;
  int                       de;
  int                       sign;
  int                       mantdigs;
  int                       nid;
  int                       nfd;
  char*                     expbeg;
  int                       explen;
  unsigned int              expwid;

  while( repetition-- )
  {
    if( ++iolist_index > iolistlen )
      IOEXCEPTION( TEXTFAIL, IOLIST_EXHAUSTED );  

    io = *iolistptr++;
    width =  dynamicwid ? get_field_width() : clausewidth;

    switch( convcode )
    {
    case DefaultConv:
      switch( io.__descr )
      {
      case __IO_ByteVal:
        longval = io.__t.__valbyte; 
        goto signed_conversion;
      case __IO_UByteVal:
        ulongval = io.__t.__valubyte; 
        goto unsigned_conversion;
      case __IO_IntVal:
        longval = io.__t.__valint; 
        goto signed_conversion;
      case __IO_UIntVal:
        ulongval = io.__t.__valuint; 
        goto unsigned_conversion;
      case __IO_LongVal:
        longval = io.__t.__vallong; 
        goto signed_conversion;
      case __IO_ULongVal:
        ulongval = io.__t.__valulong; 
        goto unsigned_conversion;

      case __IO_BoolVal:
        switch( io.__t.__valbool )
        {
        case 0:
          itembeg = "FALSE";
          itemlen = 5;
          goto move_item;
        case 1:
          itembeg = "TRUE";
          itemlen = 4;
          goto move_item;
        default:
          IOEXCEPTION( TEXTFAIL, BOOL_CONVERSION_ERROR );
        }
 
      case __IO_CharVal:
        itembeg = &io.__t.__valchar;
        itemlen = 1;
        goto move_item;
  
      case __IO_SetVal:
        /* locate name string using set mode name table */
        itembeg = 0;
        
        if( (etptr = io.__t.__valset.name_table) )
          while( etptr->name )
	  {
            if( etptr->value == io.__t.__valset.value )
	    {
              itembeg = etptr->name;
              itemlen = strlen( itembeg );
              goto move_item;
            }
            etptr++;
          }
       IOEXCEPTION( TEXTFAIL, SET_CONVERSION_ERROR ); 

      case __IO_CharVaryingLoc:
        {
          unsigned short l;
          itembeg = (char*)io.__t.__loccharstring.string;
          MOV2(&l,itembeg);
          itembeg += 2;
          itemlen = l;
          goto move_item;
        }

      case __IO_CharStrLoc:
        itembeg = io.__t.__loccharstring.string;
        itemlen = io.__t.__loccharstring.string_length;
        goto move_item;

      case __IO_BitStrLoc:
        itemlen = io.__t.__loccharstring.string_length;
        itembeg = io.__t.__loccharstring.string;

        if( !width )
          width = itemlen;

        /* check remaining space */
        if( actual_index + width > iostrlen )
          IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );

        if( itemlen == width )
          bitput( iostrptr + actual_index, itembeg, 0, itemlen );
        else
          if( itemlen < width )
            if( leftadjust )
              memset( bitput( iostrptr + actual_index, itembeg, 0, itemlen )
                      + itemlen,
                      paddingchar, width - itemlen );
            else
              bitput( memset( iostrptr + actual_index, 
                              paddingchar, width - itemlen )
                      + width - itemlen,
                      itembeg, itemlen - width, itemlen );
          else
            if( overflowev )
              memset( iostrptr + actual_index, '*', width );
            else
              if( leftadjust )
                bitput( iostrptr + actual_index, itembeg, 0, width );
              else
                bitput( iostrptr + actual_index, itembeg, 
                        itemlen - width, itemlen );
        goto adjust_index;

      case __IO_RealVal:
        doubleval = io.__t.__valreal;
        mantdigs = FMANTDIGS;
        goto fixed_point_conversion;
      case __IO_LongRealVal:
        doubleval = io.__t.__vallongreal;
        mantdigs = DBL_DIG;
        goto fixed_point_conversion;
        break;

      default:
        IOEXCEPTION( TEXTFAIL, INVALID_IO_LIST );
      }

    case HexConv:
    case OctalConv:
    case BinaryConv:
      switch( io.__descr )
      {
      case __IO_ByteVal:
      case __IO_UByteVal:
        ulongval = io.__t.__valubyte; 
        break;
      case __IO_IntVal:
      case __IO_UIntVal:
        ulongval = io.__t.__valuint; 
        break;
      case __IO_LongVal:
      case __IO_ULongVal:
        ulongval = io.__t.__valulong; 
        break;
      default:
        IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
      }
      itembeg = myultoa( ulongval, itembuf, base[convcode] );
      itemlen = strlen( itembeg );
      goto move_item;
  
    case ScientConv:
      switch( io.__descr )
      {
      case __IO_RealVal:
        doubleval = io.__t.__valreal;
        mantdigs = FMANTDIGS;
        if( !fractiondef )
          fractionwidth = FMANTDIGS - 1;
        goto scientific_conversion;
      case __IO_LongRealVal:
        doubleval = io.__t.__vallongreal;
        mantdigs = DBL_DIG;
        if( !fractiondef )
          fractionwidth = DBL_DIG - 1;
        goto scientific_conversion;
        break;
      default:
        IOEXCEPTION( TEXTFAIL, CONVCODE_MODE_MISFIT );
      }
    }

fixed_point_conversion: ;
    itembeg = mydtoa( doubleval, mantdigs, &de, &sign );
    if( fractiondef && de >= -fractionwidth - 1
        && -fractionwidth > de - mantdigs )
      itembeg = dround( itembeg, de + fractionwidth + 1, &de );

    nid = de >= 0 ? de : 0;
    nfd = fractiondef ? fractionwidth 
                      : ( de + 1 - mantdigs > 0 ? 0 : mantdigs - de - 1 );
    itemlen = ( sign < 0 ? 1 : 0 ) + 2 + nid + nfd;
#if _TEXTIO_DEBUG_
printf( "fixed item length %d\n", itemlen );
#endif
    if( !width )
      width = itemlen;
#if _TEXTIO_DEBUG_
printf( "fixed item width %d\n", width );
#endif
    /* check remaining space */
    if( actual_index + width > iostrlen )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );

    if( itemlen == width )
      fixput( iostrptr + actual_index, itembeg, 
              1, itemlen, sign, de, de - mantdigs, nid, nfd );
    else
      if( itemlen < width )
        if( leftadjust )
          memset( fixput( iostrptr + actual_index, itembeg, 
                          1, itemlen, sign, de, de - mantdigs, nid, nfd )
                  + itemlen,
                  paddingchar, width - itemlen );
        else
          fixput( memset( iostrptr + actual_index, 
                          paddingchar, width - itemlen )
                  + width - itemlen,
                  itembeg, 1, itemlen, sign, de, de - mantdigs, nid, nfd );
      else
        if( overflowev )
          memset( iostrptr + actual_index, '*', width );
        else
          if( leftadjust )
            fixput( iostrptr + actual_index, itembeg, 
                    1, width, sign, de, de - mantdigs, nid, nfd );
          else
            fixput( iostrptr + actual_index, itembeg, 
                    itemlen - width + 1, itemlen,
                    sign, de, de - mantdigs, nid, nfd );
    goto adjust_index;

scientific_conversion: ;
    itembeg = mydtoa( doubleval, mantdigs, &de, &sign );

    if( fractiondef && fractionwidth < mantdigs )
      itembeg = dround( itembeg, fractionwidth + 1, &de );

    expbeg = myultoa( abs(de), itembuf, 10 );
    explen = strlen( expbeg );

    expwid = explen > exponentwidth ? explen : exponentwidth;
    itemlen = ( sign < 0 ? 1 : 0 ) + 2 + fractionwidth + 2 + expwid;
#if _TEXTIO_DEBUG_
printf( "floating item length %d, fraction %d, exponent %d\n", 
        itemlen, fractionwidth, expwid );
#endif
    if( width == 0 )
      width = itemlen;
#if _TEXTIO_DEBUG_
printf( "floating item width %d\n", width );
#endif
    /* check remaining space */
    if( actual_index + width > iostrlen )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );

    if( itemlen == width )
      sciput( iostrptr + actual_index, itembeg, expbeg, 
              1, itemlen, sign, de, expwid );
    else
      if( itemlen < width )
        if( leftadjust )
          memset( sciput( iostrptr + actual_index, itembeg, expbeg,
                          1, itemlen, sign, de, expwid )
                  + itemlen,
                  paddingchar, width - itemlen );
        else
          sciput( memset( iostrptr + actual_index, 
                          paddingchar, width - itemlen )
                  + width - itemlen,
                  itembeg, expbeg, 1, itemlen, sign, de, expwid );
      else
        if( overflowev )
          memset( iostrptr + actual_index, '*', width );
        else
          if( leftadjust )
            sciput( iostrptr + actual_index, itembeg, expbeg,
                    1, width, sign, de, expwid );
          else
            sciput( iostrptr + actual_index, itembeg, expbeg,
                    itemlen - width + 1, itemlen,
                    sign, de, expwid );
    goto adjust_index;

signed_conversion: ;   
    if( longval >= 0 )
      itembeg = myultoa( longval, itembuf, 10 );
    else
    {
      itembuf[0] = '-';
      myultoa( -longval, itembuf+1, 10 );
      itembeg = itembuf;
    }  
    itemlen = strlen( itembeg );
    goto move_item;

unsigned_conversion: ;
    itembeg = myultoa( ulongval, itembuf, 10 );
    itemlen = strlen( itembeg );
    goto move_item;

move_item: ;
    if( !width )
      width = itemlen;

    /* check remaining space */
    if( actual_index + width > iostrlen )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );

    /* move item, filling or truncating or overflow-evidencing */
    if( itemlen == width )
      memcpy( iostrptr + actual_index, itembeg, itemlen );
    else
      if( itemlen < width )
        if( leftadjust )
          memset( memcpy( iostrptr + actual_index, itembeg, itemlen )
                  + itemlen,
                  paddingchar, width - itemlen );
        else
          memcpy( memset( iostrptr + actual_index, 
                          paddingchar, width - itemlen )
                  + width - itemlen,
                  itembeg, itemlen );
      else
        if( overflowev )
          memset( iostrptr + actual_index, '*', width );
        else
          if( leftadjust )
            memcpy( iostrptr + actual_index, itembeg, width );
          else
            memcpy( iostrptr + actual_index, 
                    itembeg + itemlen - width, width );

  /*
   *  adjust.
   */
adjust_index: ;
  actual_index += width;
  if( actual_index > maximum_index )
    maximum_index = actual_index;
  }
}

static
void outedit( void )
{
  int nchars;

  if( dynamicwid )
    clausewidth = get_field_width();
  switch( editcode )
  { 
  case SpaceSkip:
    nchars = repetition*clausewidth;
    if( actual_index + nchars > iostrlen )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );
    memset( iostrptr + actual_index, ' ', nchars );
    actual_index += nchars;
    if( actual_index > maximum_index )
      maximum_index = actual_index;
    break;

  case SkipLeft:
    nchars = repetition*clausewidth;
    if(  actual_index - nchars < 0 )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );
    actual_index -= nchars;
    break;

  case SkipRight:
    nchars = repetition*clausewidth;
    if( actual_index + nchars > iostrlen )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );
    actual_index += nchars;
    if( actual_index > maximum_index )
    {
      memset( iostrptr + maximum_index, ' ', actual_index - maximum_index );
      maximum_index = actual_index;
    }
    break;
  
  case Tabulation:
    if( clausewidth >= iostrlen )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );
    actual_index = clausewidth;
    if( actual_index > maximum_index )
    {
      memset( iostrptr + maximum_index, ' ', actual_index - maximum_index );
      maximum_index = actual_index;
    }  
    break;
  }
}


static
void inpioctrl( void )
{
  unsigned short hlen;
  if( !textptr )
    IOEXCEPTION( TEXTFAIL, IO_CONTROL_NOT_VALID );
  if( iocode != EndPage )
  {
    jmp_buf ioerror;
    unsigned long info;

    if (textptr->access_sub->association)
      {
	if( (info = setjmp( ioerror )) )
	  IOEXCEPTION( info>>16, info & 0xffff );    
	while( repetition-- )
	  {
	    __readrecord( textptr->access_sub, textindex,
			 (char*)textptr->text_record, 
			 __FILE__, __LINE__ );
	    actual_index = 0;
	    MOV2(&hlen,&textptr->text_record->len);
	    iostrlen = hlen;
	  }
      }
    else
      IOEXCEPTION (NOTCONNECTED, IS_NOT_CONNECTED);
  }
}

/* specify pre/post in the order "/+-?!" */
static
char* pre_char =  "\0\f\0\r\0"; /* Z.200: "\n\f\0\n\0" */
static
char* post_char = "\n\n\r\0\0"; /* Z.200: "\r\r\r\0\0" */

static
void outioctrl( void )
{
  Association_Mode* assoc;
  unsigned short hlen;
  if( !textptr )
    IOEXCEPTION( TEXTFAIL, IO_CONTROL_NOT_VALID );
  if( (assoc = textptr->access_sub->association) )
  {
    jmp_buf ioerror;
    unsigned long info;
    if( (info = setjmp( ioerror )) )
      IOEXCEPTION( info>>16, info & 0xffff );    

    while( repetition-- )
    {
      if( iocode != EndPage )
      {
        if( TEST_FLAG( assoc, IO_FIRSTLINE ) )
        {
          CLR_FLAG( assoc, IO_FIRSTLINE );
          assoc->ctl_pre = '\0';
        }
        else
        {
          if( TEST_FLAG( assoc, IO_FORCE_PAGE ) )
          {
            CLR_FLAG( assoc, IO_FORCE_PAGE );
            assoc->ctl_pre = '\f';
	  }
          else
            assoc->ctl_pre = pre_char[iocode];
        }
        assoc->ctl_post = post_char[iocode];
        hlen = actual_index;
        MOV2(&textptr->text_record->len,&hlen);
        __writerecord( textptr->access_sub, textindex,
                       (char*)textptr->text_record,
                       textptr->text_record->len,
                       __FILE__, __LINE__ );
        hlen = actual_index = 0;
        MOV2(&textptr->text_record->len,&hlen);
      }
      else if( !TEST_FLAG( textptr, IO_FIRSTLINE ) )
	SET_FLAG( textptr, IO_FORCE_PAGE );
      assoc->ctl_pre = assoc->ctl_post = '\0';
    }
  }
  else
    IOEXCEPTION (NOTCONNECTED, IS_NOT_CONNECTED);
}

static
void (**actionptr)( void );
static
void (*readactions[])( void ) = { inpconv, inpedit, inpioctrl };
static
void (*writeactions[])( void ) = { outconv, outedit, outioctrl };


static
void emitstr( char* begtxt, char* endtxt )
{  
  char c;
  int  nchars = endtxt - begtxt;
  if( actual_index + nchars > iostrlen )
      IOEXCEPTION( TEXTFAIL, TEXT_LOC_OVERFLOW );
  memcpy( iostrptr + actual_index, begtxt, nchars );
  actual_index += nchars;
  if( actual_index > maximum_index )
    maximum_index = actual_index;
}

static
void scanstr( char* begtxt, char* endtxt )
{  
  int  nchars = endtxt - begtxt;
  if( actual_index + nchars > iostrlen )
    IOEXCEPTION( TEXTFAIL, NO_CHARS_FOR_TEXT );
  if( strncmp( iostrptr + actual_index, begtxt, nchars ) )
    IOEXCEPTION( TEXTFAIL, FORMAT_TEXT_MISMATCH );
  actual_index += nchars;
}

void (*ftextptr) ( char*, char* );

static
formatexit_t scanformcont( char* fcs, int len,
                           char** fcsptr, int* lenptr )
{
  char          curr; 
  fcsstate_t    state  = FormatText;
  unsigned long buf;
  int           dig;
  acttype_t     action;
  char*         begtxt = fcs;

  while( len-- )
  {
    curr = *fcs++;
    switch( state )
    {
    case FormatText: 
      if( curr == '%' )
      {
        ftextptr( begtxt, fcs-1 );
        state = FirstPercent;
      }
      break;

after_first_percent: ;
    case FirstPercent: 
      if( curr == '%' )
      {
        state = FormatText;
        begtxt = fcs - 1;
        break;
      }
      if( curr == ')' )
      {
        *lenptr = len;
        *fcsptr = fcs;
        return EndAtParen;
      }
      if( isDEC(curr) )
      {
        state = RepFact;
        repetition = curr - '0';
        break;
      }

      repetition = 1; 

test_for_control_codes: ;
      if( isCVC(curr) )
      {
        state = ConvClause;
        action = ConvAct;
        convcode = strchr( CONVERSIONCODES, curr ) - CONVERSIONCODES;
        leftadjust = False;
        overflowev = False;
        dynamicwid = False;
        paddingdef = False;
        paddingchar = ' ';
        fractiondef = False;
        /* fractionwidth = 0; default depends on mode ! */
        exponentdef = False;
        exponentwidth = 3;
        clausewidth = 0;        
        break;        
      }
      if( isEDC(curr) )
      {
        state = EditClause;
        action = EditAct;
        editcode = strchr( EDITCODES, curr ) - EDITCODES;
        dynamicwid = False;
        clausewidth = editcode == Tabulation ? 0 : 1;        
        break;        
      }
      if( isIOC(curr) )
      {
        state = ClauseEnd;
        action = IOAct;
        iocode = strchr( IOCODES, curr ) - IOCODES;
        break;        
      }
      if( curr == '(' )
      {
        unsigned long times = repetition;
        int  cntlen;
        char* cntfcs;         
        while( times-- )
        {        
          if( scanformcont( fcs, len, &cntfcs, &cntlen ) != EndAtParen )
            IOEXCEPTION( TEXTFAIL, UNMATCHED_OPENING_PAREN );
        }
        fcs = cntfcs;
        len = cntlen;
        state  = FormatText;
        begtxt = fcs;
        break;
      }
      IOEXCEPTION( TEXTFAIL, BAD_FORMAT_SPEC_CHAR );

    case RepFact:
      if( isDEC(curr) )
      {
        dig = curr - '0';
        if( repetition > (ULONG_MAX - dig)/10 )
          IOEXCEPTION( TEXTFAIL, REPFAC_OVERFLOW );
        repetition = repetition*10 + dig;
        break;
      }
      goto test_for_control_codes;

    case ConvClause:
      if( isDEC(curr) )
      {
        state = ClauseWidth;
        clausewidth = curr - '0';
        break;
      }
      if( curr == 'L' )  
      {
        if( leftadjust ) 
          IOEXCEPTION( TEXTFAIL, DUPLICATE_QUALIFIER );
        leftadjust = True;
        break;
      }
      if( curr == 'E' )
      {
        if( overflowev ) 
          IOEXCEPTION( TEXTFAIL, DUPLICATE_QUALIFIER );
        overflowev = True;
        break;
      }
      if( curr == 'P' )
      {
        if( paddingdef ) 
          IOEXCEPTION( TEXTFAIL, DUPLICATE_QUALIFIER );
        paddingdef = True;
        state = CatchPadding;
        break;
      }

test_for_variable_width: ;
      if( curr == 'V' )
      {
        dynamicwid = True;
        state = AfterWidth;
        break;
      }
      goto test_for_fraction_width;

    case ClauseWidth:
      if( isDEC(curr) )
      {
        dig = curr - '0';
        if( clausewidth > (ULONG_MAX - dig)/10 )
          IOEXCEPTION( TEXTFAIL, CLAUSE_WIDTH_OVERFLOW );
        clausewidth = clausewidth*10 + dig;
        break;
      }
      /* fall through */

test_for_fraction_width: ;
    case AfterWidth:
      if( curr == '.' )
      {
        if( convcode != DefaultConv && convcode != ScientConv )
          IOEXCEPTION( TEXTFAIL, NO_FRACTION );
        fractiondef = True;
        state = FractWidth;
        break;
      }
      goto test_for_exponent_width;

    case FractWidth:
      if( isDEC( curr ) )
      {
        state = FractWidthCont;
        fractionwidth = curr - '0';
        break;
      }
      else
        IOEXCEPTION( TEXTFAIL, NO_FRACTION_WIDTH );

    case FractWidthCont:
      if( isDEC( curr ) )
      {
        dig = curr - '0';
        if( fractionwidth > (ULONG_MAX - dig)/10 )
          IOEXCEPTION( TEXTFAIL, FRACTION_WIDTH_OVERFLOW );
        fractionwidth = fractionwidth*10 + dig;
        break;
      }
             
test_for_exponent_width: ;
      if( curr == ':' )
      {
        if( convcode != ScientConv )
          IOEXCEPTION( TEXTFAIL, NO_EXPONENT );
        exponentdef = True;
        state = ExpoWidth;
        break;
      }
      goto test_for_final_percent;

    case ExpoWidth:
      if( isDEC( curr ) )
      {
        state = ExpoWidthCont;
        exponentwidth = curr - '0';
        break;
      }
      else
        IOEXCEPTION( TEXTFAIL, NO_EXPONENT_WIDTH );

    case ExpoWidthCont:
      if( isDEC( curr ) )
      {
        dig = curr - '0';
        if( exponentwidth > (ULONG_MAX - dig)/10 )
          IOEXCEPTION( TEXTFAIL, EXPONENT_WIDTH_OVERFLOW );
        exponentwidth = exponentwidth*10 + dig;
        break;
      }
      /* fall through  */

test_for_final_percent: ;
    case ClauseEnd:
      if( curr == '%' )
      {
        state = LastPercent;
        break;
      }
 
  do_the_action: ;
      actionptr[action]();
      state = FormatText;
      begtxt = fcs - 1;
      break;

    case CatchPadding:
      paddingchar = curr;
      state = ConvClause;
      break;

    case EditClause:
      if( isDEC(curr) )
      {
        state = ClauseWidth;
        clausewidth = curr - '0';
        break;
      }
      goto test_for_variable_width; 

    case LastPercent:
      actionptr[action]();
      if( curr == '.' )
      {
        state = FormatText;
        begtxt = fcs;
        break;
      }
      goto after_first_percent;

    default:
      IOEXCEPTION( TEXTFAIL, INTERNAL_ERROR );
    }
  }
  switch( state )
  {
  case FormatText:
    ftextptr( begtxt, fcs );
    break;
  case FirstPercent: 
  case LastPercent:
  case RepFact:
  case FractWidth:
  case ExpoWidth:
    IOEXCEPTION( TEXTFAIL, BAD_FORMAT_SPEC_CHAR );
  case CatchPadding:
    IOEXCEPTION( TEXTFAIL, NO_PAD_CHAR );
  default:
    actionptr[action]();
  }

  *lenptr = len;
  *fcsptr = fcs;
  return NormalEnd;
}

static
void
__read_format (char*           fmtptr,
               int             fmtlen,
               __tmp_IO_list*  ioptr,
               int             iolen,
               void*           inpptr,
               int             inplen )
{
  formatexit_t res;
  unsigned short l;

  iostrptr = (char*)inpptr;
  iostrlen = inplen;

  /* initialisation */
  iolist_index = 0;
  iolistptr    = ioptr; 
  iolistlen    = iolen;
  
  actionptr = readactions;
  ftextptr = scanstr;
     
  if( (res = scanformcont( fmtptr, fmtlen, &fmtptr, &fmtlen )) == EndAtParen )
    IOEXCEPTION( TEXTFAIL, UNMATCHED_CLOSING_PAREN );

  if( iolist_index != iolen )
    IOEXCEPTION( TEXTFAIL, EXCESS_IOLIST_ELEMENTS );

  return;
}

void
__readtext_f( Text_Mode*      the_text_loc,
              signed long     the_index,
              char*           fmtptr,
              int             fmtlen,
              __tmp_IO_list*  ioptr,
              int             iolen,
              char*           file,
              int             line )
{
  unsigned long info;

  if( (info = setjmp( __io_exception )) )
    CHILLEXCEPTION( file, line, info>>16, info & 0xffff );

  textptr       = the_text_loc;
  textrecptr    = textptr->text_record;
  actual_index  = textptr->actual_index;
  textindex     = the_index;

  __read_format ( fmtptr, fmtlen, ioptr, iolen,
                  (char*)textrecptr + 2, textptr->text_record->len );
  textptr->actual_index = actual_index;
}

void
__readtext_s( void*           string_ptr,
              int             string_len,
              char*           fmtptr,
              int             fmtlen,
              __tmp_IO_list*  ioptr,
              int             iolen,
              char*           file,
              int             line )
{
  int info;

  if( (info = setjmp( __io_exception )) )
    CHILLEXCEPTION( file, line, info>>16, info & 0xffff );

  textptr      = NULL;
  actual_index = 0;

  __read_format ( fmtptr, fmtlen,  ioptr, iolen, string_ptr, string_len );
}

static
void
__write_format (char*           fmtptr,
                int             fmtlen,
                __tmp_IO_list*  ioptr,
                int             iolen,
                void*           outptr,
                int             outlen )
{
  formatexit_t res;
  unsigned short l;

  /* initialisation */
  maximum_index = actual_index;
  iolist_index = 0;
  
  actionptr = writeactions;
  ftextptr  = emitstr;
  iolistptr = ioptr; 
  iolistlen = iolen;
  iostrptr  = (char *)outptr + 2;
  iostrlen  = outlen;

  if( (res = scanformcont( fmtptr, fmtlen, &fmtptr, &fmtlen )) == EndAtParen )
    IOEXCEPTION( TEXTFAIL, UNMATCHED_CLOSING_PAREN );

  if( iolist_index != iolen )
    IOEXCEPTION( TEXTFAIL, EXCESS_IOLIST_ELEMENTS );

  /* set length of output string */
#if _TEXTIO_DEBUG_
  printf( "maximum index = %d\n", maximum_index );
#endif
  l = maximum_index;
  MOV2(outptr,&l);
  return;
}

void
__writetext_f( Text_Mode*      the_text_loc,
               signed long     the_index,
               char*           fmtptr,
               int             fmtlen,
               __tmp_IO_list*  ioptr,
               int             iolen,
               char*           file,
               int             line )
{
  int info;

  if( (info = setjmp( __io_exception )) )
    CHILLEXCEPTION( file, line, info>>16, info & 0xffff );

  textptr       = the_text_loc;
  textrecptr    = the_text_loc->text_record;
  textindex     = the_index;
  iolistptr     = ioptr; 
  iolistlen     = iolen;

  actual_index = textptr->actual_index;
  __write_format ( fmtptr, fmtlen, ioptr, iolen,
                   textrecptr, textptr->access_sub->reclength - 2 );
  textptr->actual_index = actual_index;
}

void
__writetext_s( void*           string_ptr,
               int             string_len,
               char*           fmtptr,
               int             fmtlen,
               __tmp_IO_list*  ioptr,
               int             iolen,
               char*           file,
               int             line )
{
  int info;

  if( (info = setjmp( __io_exception )) )
    CHILLEXCEPTION( file, line, info>>16, info & 0xffff );

  textptr      = NULL;
  actual_index = 0;

  __write_format ( fmtptr, fmtlen, ioptr, iolen, string_ptr, string_len );
}
