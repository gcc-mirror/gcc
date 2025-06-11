/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* COBOL intrinsic functions.
 *
 * In general, the parameters to these functions are cblc_field_t pointers
 * along with an offset, size, and for some functions the "allflags", which
 * indicate that the variable is a table that was referenced as TABL(ALL)
 */

#include <langinfo.h>

#include <cctype>
#include <cmath>
#include <cstring>
#include <ctime>

#include <algorithm>
#include <vector>

#include "config.h"
#include "libgcobol-fp.h"

#include "ec.h"
#include "common-defs.h"
#include "io.h"
#include "gcobolio.h"
#include "libgcobol.h"
#include "charmaps.h"


#pragma GCC diagnostic ignored "-Wformat-truncation"

#define JD_OF_1601_01_02 2305812.5

#define WEIRD_TRANSCENDENT_RETURN_VALUE GCOB_FP128_LITERAL (0.0)
#define NO_RDIGITS (0)

struct cobol_tm
  {
  int YYYY;           // 1601-9999
  int MM;             // 01-12
  int DD;             // 01-28,29,30,31
  int hh;             // 00-23
  int mm;             // 00-59
  int ss;             // 00-59
  int nanoseconds;    // 0 through 999,999,999
  int tz_offset;      // +/- 1359
  int week_of_year;   // 01 - 52,53
  int day_of_year;    // 001-365, 366
  int day_of_week;    // 0-6; 0 being Monday
  int days_in_year;   // 365,366
  int weeks_in_year;  // 52,53
  int ZZZZ;           // Alternate year, when Jan 4 is Mon, Tue, or Wednesday
  };

static int is_leap_year(int);

typedef char * PCHAR;

static void
trim_trailing_spaces(PCHAR left, PCHAR &right)
  {
  while( right > left )
    {
    if( *(right-1) != internal_space )
      {
      break;
      }
    right -= 1;
    }
  }

static bool
is_zulu_format(PCHAR left, PCHAR &right)
  {
  bool retval = false;
  if( right > left )
    {
    retval = std::toupper((unsigned char)*(right-1)) == internal_Z;
    }
  return retval;
  }

static double
YMD_to_JD(int Y, int M, int D)
  {
  // Calculates the Julian Day

  if( M <= 2 )
    {
    Y -= 1 ;
    M += 12;
    }
  double A = floor(Y/100.);
  double B = 2. - A + floor(A/4.);

  double JD;
  JD = floor(365.25 * double(Y + 4716) + floor((30.6001 * double(M+1)))) + D + B -1524.5 ;

  return JD;
  }

static void
JD_to_YMD(int &YY, int &MM, int &DD, double JD)
  {
  JD += 0.5;
  double Z = floor(JD);
  double F = JD - Z;
  double A;
  if( Z < 2299161.0 )
    {
    A = Z;
    }
  else
    {
    double alpha = floor( (Z-1867216.25) / 36524.25 ) ;
    A = Z + 1.0 + alpha - floor(alpha/4.0);
    }
  double B = A + 1524;
  double C = floor( (B - 122.1)/365.25 );
  double D = floor( 365.25 * C );
  double E = floor( (B-D)/30.6001 );

  DD = (int)( B - D - floor(30.6001 * E) + F );
  MM = (int)( E < 14 ? E - 1 : E - 13 );
  YY = (int)( MM > 2 ? C - 4716 : C - 4715 );
  }

static int
JD_to_DOW(double JD)
  {
  // Converts a Julian Day to 0 through 6, where
  // 0 is Monday.
  // 2415020.50000 is noon on 1900-01-01, which was a Monday
  return ((int)(JD-0.5)+1)%7;
  }

#define DATE_STRING_BUFFER_SIZE 23

static
char *
timespec_to_string(char *retval, struct cbl_timespec &tp)
  {
  /*
  Returns a 21-character string:

   1 -  4 Four numeric digits of the year in the Gregorian calendar
   5 -  6 Two numeric digits of the month of the year, in the range 01 through 12
   7 -  8 Two numeric digits of the day of the month, in the range 01 through 31
   9 - 10 Two numeric digits of the hours past midnight, in the range 00 through 23
  11 - 12 Two numeric digits of the minutes past the hour, in the range 00 through 59
  13 - 14 Two numeric digits of the seconds past the minute, in the range 00 through 59
  15 - 16 Two numeric digits of the hundredths of a second past the second, in the range
       17 Either the character '-' or the character '+'.
  18 - 19 If character position 17 is '-', two numeric digits are returned in the range 00
          through 12 indicating the number of hours that the reported time is behind
          Greenwich mean time.

          If character position 17 is '+', two numeric digits are
          returned in the range 00 through 13 indicating the number of hours that the
          reported time is ahead of Greenwich mean time. If character position 17 is '0', the
          value 00 is returned.
  20 - 21 Two numeric digits are returned in the range 00 through 59 indicating the number
          of additional minutes that the reported time is ahead of or behind Greenwich
          mean time, depending on whether character position 17
  */

  const int size_of_buffer = DATE_STRING_BUFFER_SIZE;
  const int offset_to_hundredths = 14;
  const long nanoseconds_to_hundredths = 10000000;

  // Convert the nanosecond fraction to hundredths of a second:
  char achCentiseconds[3];
  snprintf(achCentiseconds, 3, "%2.2ld", (tp.tv_nsec/nanoseconds_to_hundredths) );

  // Convert the epoch seconds to broken-down time:
  struct tm tm = {};

  if( false )
    {
    // With a forced date/time, eliminate local influences
    gmtime_r(&tp.tv_sec, &tm);
    }
  else
    {
    localtime_r(&tp.tv_sec, &tm);
    }

  // Format the time as per COBOL specifications, leaving two spaces for the
  // hundredths of seconds:
  strftime(retval, size_of_buffer, "%Y%m%d%H%M%S  %z", &tm);

  // Copy the 100ths into place:
  memcpy(retval+offset_to_hundredths, achCentiseconds, 2);

  return retval;
  }

static
void
string_to_dest(cblc_field_t *dest, const char *psz)
  {
  size_t dest_length = dest->capacity;
  size_t source_length = strlen(psz);
  size_t length = std::min(dest_length, source_length);
  memset(dest->data, internal_space, dest_length);
  memcpy(dest->data, psz, length);
  }

struct input_state
  {
  size_t nsubscript;
  bool   *subscript_alls;
  size_t *subscripts;
  size_t *subscript_limits;
  bool    done;

  void allocate(size_t N)
    {
    nsubscript = N;
    if(N)
      {
      subscript_alls   = static_cast<bool   *>(malloc(nsubscript));
      subscripts       = static_cast<size_t *>(malloc(nsubscript));
      subscript_limits = static_cast<size_t *>(malloc(nsubscript));
      massert(subscript_alls);
      massert(subscripts);
      massert(subscript_limits);
      }
    done = false;
    }
  void deallocate()
    {
    if(nsubscript)
      {
      free(subscript_alls);
      free(subscripts);
      free(subscript_limits);
      }
    }
  };

struct refer_state_for_all
  {
  size_t nflags;
  size_t coefficients   [MAXIMUM_TABLE_DIMENSIONS];
  size_t capacities     [MAXIMUM_TABLE_DIMENSIONS];
  size_t limits         [MAXIMUM_TABLE_DIMENSIONS];
  };

static
void
build_refer_state_for_all(  refer_state_for_all &state,
                            cblc_field_t *field,
                            int    flags)
  {
  memset(&state, 0, sizeof(refer_state_for_all) );
  if( flags & REFER_T_ALL_FLAGS_MASK )
    {
    // At this point, refer points to the very first element of
    // an array specification that includes at least one ALL subscript.  At
    // this time, those ALLs were calculated as if they had been replaced
    // with one.

    // We are going to walk the reference up to its ultimate parent, picking
    // up what we need along the way.

    size_t current_bit = 1;
    size_t current_index = 0;
    cblc_field_t *current_sizer = field;
    while( current_sizer )
      {
      while( current_sizer && !current_sizer->occurs_upper )
        {
        // current_sizer isn't a table, which isn't unusual.
        current_sizer = current_sizer->parent;
        }

      if( !current_sizer )
        {
        // We have found all of the elements in this data description
        // that have OCCURS clauses
        break;
        }

      // We are sitting on an occurs clause:

      if( current_bit & flags )
        {
        // It is an ALL subscript:
        state.nflags += 1;
        state.coefficients[current_index] = 1;
        state.capacities[current_index] = current_sizer->capacity;
        state.limits[current_index] = current_sizer->occurs_upper;
        current_index += 1 ;
        }

      current_bit <<= 1;
      current_sizer = current_sizer->parent;
      }
    }
  }

static
bool
update_refer_state_for_all( refer_state_for_all &state,
                            cblc_field_t *field)
  {
  bool retval = false;  // Means there is nothing left

  for(size_t i=0; i<state.nflags; i++)
    {
    state.coefficients[i] += 1;
    field->data += state.capacities[i];
    if( state.coefficients[i] <= state.limits[i] )
      {
      // This coefficient is within range:
      retval = true;
      break;
      }

    // We have used up this coefficient.

    // Remove the effects of incrementing this coefficient:
    field->data -= state.limits[i] * state.capacities[i];
    // Reset the coefficient back to one:
    state.coefficients[i] = 1;

    // And continue on to the next coefficient.
    }

  return retval;
  }

static
int
year_to_yyyy(int arg1, int arg2, int arg3)
  {
  // See ISO/IEC 2014-1989 section 15.93 for a detailed description of the
  // sliding window calculation
  int max_year = arg2 + arg3;
  int retval;
  if( max_year % 100 >= arg1 )
    {
    retval = arg1 + 100 * (max_year/100);
    }
  else
    {
    retval = arg1 + 100 * (max_year/100 - 1);
    }
  return retval;
  }

static
double
get_value_as_double_from_qualified_field( const cblc_field_t *input,
                                          size_t input_o,
                                          size_t input_s)
  {
  double retval;
  int rdigits;

  switch( input->type )
    {
    case FldFloat:
      fprintf(stderr, "get_value_as_double_from_qualified_field(): Hey!"
                      "  We got an unexpected float in intrinsic.cc!\n");
      exit(1);
      break;

    default:
      retval = __gg__binary_value_from_qualified_field(&rdigits,
                                                        input,
                                                        input_o,
                                                        input_s);
      for(int i=0; i<rdigits; i++)
        {
        retval /= 10.0;
        }
      break;
    }

  return retval;
  }

static
GCOB_FP128 kahan_summation(size_t ncount,
                          cblc_field_t **source,
                    const size_t        *source_o,
                    const size_t        *source_s,
                    const int           *flags,
                          size_t        *k_count)
  {
  // We use compensated addition.  Look up Kahan summation.

  //  In the Kahan summation algorithm, the C value accumulates small errors
  //  Algebraically, it should be zero.  So, "volatile" is an attempt to prevent
  //  an aggressive optimizing compiler from just making it go away.

  *k_count = 0;
  GCOB_FP128 sum = 0;
  volatile GCOB_FP128 kahan_c = 0;
  GCOB_FP128 input;
  GCOB_FP128 y;
  GCOB_FP128 t;

  for(size_t i=0; i<ncount; i++)
    {
    refer_state_for_all state;
    build_refer_state_for_all(state, source[i], flags[i]);

    for(;;)
      {
      input = __gg__float128_from_qualified_field(source[i],
                                                  source_o[i],
                                                  source_s[i]);
      y = input - kahan_c;
      t = sum + y;
      kahan_c = (t - sum) - y ;
      sum = t;
      *k_count += 1;
      if( !update_refer_state_for_all(state, source[i]) )
        {
        // There is nothing left to do.
        break;
        }
      }
    }
  return sum;
  }

static
GCOB_FP128
variance( size_t         ncount,
          cblc_field_t **source,
    const size_t        *source_o,
    const size_t        *source_s,
    const int           *flags)
  {
  // In order to avoid catastrophic cancellation, we are going to use an
  // algorithm that is a bit wasteful of time, but is described as particularly
  // robust.

  GCOB_FP128 retval = 0;
  if( ncount )
    {
    // First, we calculate the mean of the input variables, which we will use
    // as an offset in the second stage:
    size_t k_count;
    GCOB_FP128 offset = kahan_summation( ncount,
                                        source,
                                        source_o,
                                        source_s,
                                        flags,
                                        &k_count);
    offset /= k_count;

    // Next, we use Welford's algorithm on the residuals:

    size_t count = 0;
    GCOB_FP128 mean  = 0;
    GCOB_FP128 M2    = 0;
    GCOB_FP128 delta;
    GCOB_FP128 delta2;
    GCOB_FP128 newValue;

    for(size_t i=0; i<ncount; i++)
      {
      refer_state_for_all state;
      build_refer_state_for_all(state, source[i], flags[i]);

      for(;;)
        {
        newValue  = __gg__float128_from_qualified_field(source[i],
                                                        source_o[i],
                                                        source_s[i]);
        newValue -= offset;

        count += 1;
        delta = newValue - mean ;
        mean += delta / count;
        delta2 = newValue - mean;
        M2 += delta * delta2;
        if( !update_refer_state_for_all(state, source[i]) )
          {
          // There is nothing left to do.
          break;
          }
        }
      }

    retval = M2 / count;
    }
  return retval;
  }

static
void
get_all_time( char *stime,
              const struct cobol_tm &ctm)
  {
  // This routine represents a universal source for all output formatted date
  // and formatted time functions.  Messeth not with the format, for all the
  // calling routines are counting on it.
  //
  //                   1111111111222222222233333333334
  //         01234567890123456789012345678901234567890
  // Returns YYYYMMDDThhmmss.sssssssss+hhmmWwwdDDDZZZZ
  //
  // YYYY is the year
  //   MM is the month
  //   DD is the day of the month
  //   hh is the hour
  //   mm is the minute
  //   ss is the second
  //   .sssssssss are the nanoseconds
  //   +hhmm is the offset from UTC
  //   Www   is the COBOL-style week number. (See the comments and the spec)
  //   d     is the COBOL-style day-of-week 1-7, 1 being Monday
  //   DDD   is the COBOL-style day-of-year, 001 being Jan 01
  //   ZZZZ  is the alternate year.  Sometimes the first one, two, or three
  //         days of January show up in the final week of the prior year.

  sprintf(stime,
          "%4.4d%2.2d%2.2dT"  // YYYYMMSS
          "%2.2d%2.2d%2.2d"   // hhmmss
          ".%9.9d"            // .sssssssss
          "%c%2.2d%2.2d"      // +hhmm
          "W%2.2d"            // Www
          "%1d"               // DOW [1-7], 1 for Monday
          "%3.3d"             // DDD day of year, 001 - 365,366
          "%4.4d",            // ZZZZ Year for YYYY-Www-D
          ctm.YYYY,
          ctm.MM,
          ctm.DD,
          ctm.hh,
          ctm.mm,
          ctm.ss,
          ctm.nanoseconds,
          ctm.tz_offset < 0 ? '-' : '+',
          abs(ctm.tz_offset) / 60,
          abs(ctm.tz_offset) % 60,
          ctm.week_of_year,
          ctm.day_of_week+1,
          ctm.day_of_year,
          ctm.ZZZZ);
  // We might be operating in EBCDIC:
  ascii_to_internal_str(stime, strlen(stime));
  }

static
int
is_leap_year(int yyyy)
  {
  static const unsigned char leap_year_bits[50] =
    {
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x01, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x10, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x01, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    };

  static const unsigned char mask[8] =
    { 1, 2, 4, 8, 0x10, 0x20, 0x40, 0x80 };

  int days_in_year;

  int year_in_cycle = yyyy % 400;

  if( leap_year_bits[year_in_cycle/8] & mask[year_in_cycle & 0x07] )
    {
    days_in_year = 366;
    }
  else
    {
    days_in_year = 365;
    }

  return days_in_year;
  }

static
int
weeks_in_year(int YYYY)
  {
  double Jan4 = YMD_to_JD(YYYY, 1, 4);
  int dow = JD_to_DOW(Jan4);
  if( dow == 6 )
    {
    return 53;
    }
  if( dow == 5 && is_leap_year(YYYY) == 366 )
    {
    return 53;
    }
  return 52;
  }

static
void
populate_ctm_from_tm(struct cobol_tm &ctm, const struct tm &tm)
  {
  ctm.YYYY = tm.tm_year + 1900;
  ctm.MM   = tm.tm_mon  +    1;
  ctm.DD   = tm.tm_mday       ;
  ctm.hh   = tm.tm_hour;
  ctm.mm   = tm.tm_min;
  ctm.ss   = tm.tm_sec;
  ctm.days_in_year  = is_leap_year(ctm.YYYY);
  ctm.weeks_in_year = weeks_in_year(ctm.YYYY);

  double JD = YMD_to_JD(ctm.YYYY, ctm.MM, ctm.DD);
  ctm.day_of_week = JD_to_DOW(JD);

  double JD_Jan4   = YMD_to_JD(ctm.YYYY, 1, 4);
  ctm.day_of_year  = (int)(JD - (JD_Jan4-4));

  int    dow_Jan4  = JD_to_DOW(JD_Jan4);
  double adjusted_starting_date = JD_Jan4 - dow_Jan4;

  int adjusted_days = (int)(JD - adjusted_starting_date);
  if( adjusted_days >= 0 )
    {
    int week_of_year = adjusted_days/7 + 1;
    if(week_of_year > ctm.weeks_in_year)
      {
      ctm.ZZZZ = ctm.YYYY+1;
      ctm.week_of_year = 1;
      }
    else
      {
      ctm.ZZZZ = ctm.YYYY;
      ctm.week_of_year = week_of_year;
      }
    }
  else
    {
    ctm.ZZZZ = ctm.YYYY - 1;
    ctm.week_of_year = weeks_in_year(ctm.ZZZZ);
    }
  }

static
void
populate_ctm_from_JD(struct cobol_tm &ctm, double JD )
  {
  // Extract the year, month, and day
  int Y;
  int M;
  int D;
  JD += JD_OF_1601_01_02;
  JD_to_YMD(Y, M, D, JD);

  struct tm tm = {};
  tm.tm_mday = D;
  tm.tm_mon  = M-1;
  tm.tm_year = Y-1900;
  populate_ctm_from_tm(ctm, tm);
  }

static
void
populate_ctm_from_date( struct cobol_tm &ctm,
                  const cblc_field_t *pdate,
                        size_t pdate_offset,
                        size_t pdate_size)
  {
  // Get the date as an integer
  int rdigits;
  double JD = (double)__gg__binary_value_from_qualified_field(&rdigits,
                                                              pdate,
                                                              pdate_offset,
                                                              pdate_size);
  populate_ctm_from_JD(ctm, JD);
  }

static
void
populate_ctm_from_double_time(struct cobol_tm &ctm, double time)
  {
  // Get hours, minutes, and seconds
  double intpart;
  double fracpart = modf(time, &intpart);

  int hour = (int)intpart;
  int second = hour % 60;
  int minute = (hour / 60) % 60;
  hour = hour / 3600;
  ctm.ss = second;
  ctm.mm = minute;
  ctm.hh = hour;
  ctm.nanoseconds = (int)(fracpart * 1000000000 + 0.5);
  }

static
void
populate_ctm_from_time( struct cobol_tm &ctm,
                   const cblc_field_t *ptime,
                        size_t ptime_o,
                        size_t ptime_s,
                   const cblc_field_t *poffset,
                        size_t poffset_o,
                        size_t poffset_s)
  {
  double time = get_value_as_double_from_qualified_field( ptime,
                                                          ptime_o,
                                                          ptime_s);
  populate_ctm_from_double_time(ctm, time);

  if( poffset )
    {
    int rdigits;
    int value = (int)__gg__binary_value_from_qualified_field(&rdigits,
                                                              poffset,
                                                              poffset_o,
                                                              poffset_s);
    if( rdigits )
      {
      value /= __gg__power_of_ten(rdigits);
      rdigits = 0;
      }
    ctm.tz_offset = value;
    if( abs(value) >= 1440 )
      {
      exception_raise(ec_argument_function_e);
      }
    }
  else
    {
    ctm.tz_offset = 0;
    }
  }

static void
convert_to_zulu(cobol_tm &ctm)
  {
  // Get the Julian Day
  double JD = YMD_to_JD(ctm.YYYY,
                        ctm.MM,
                        ctm.DD);
  // Get the time in seconds past midnight
  double seconds_past_midnight =   ctm.hh * 3600
                                 + ctm.mm *   60
                                 + ctm.ss;
  // Subtract the UTC offset, which is given in minutes
  seconds_past_midnight -= ctm.tz_offset * 60;
  if( seconds_past_midnight < 0 )
    {
    JD -= 1;
    seconds_past_midnight += 86400;
    }
  else if( seconds_past_midnight >= 86400 )
    {
    JD += 1;
    seconds_past_midnight -= 86400;
    }
  JD -= JD_OF_1601_01_02;
  populate_ctm_from_JD(ctm, JD);
  populate_ctm_from_double_time(ctm, seconds_past_midnight);
  if( ctm.YYYY < 1601 )
    {
    ctm.YYYY = ctm.MM = ctm.DD = 0;
    }
  }

static
void
ftime_replace(char *dest,
              char const * const dest_end,
              char const *       source,
              char const * const source_end,
              char const * const ftime)
  {
  // This routine is highly dependent on the source format being correct.
  int ncount;
  const char *src;
  bool saw_decimal_point = false;
  bool saw_plus_sign = false;
  char decimal_point = __gg__get_decimal_point();
  static const int OFFSET_TO_YYYY = 0;
  static const int OFFSET_TO_MM   = 4;
  static const int OFFSET_TO_DD   = 6;
  static const int OFFSET_TO_HOUR = 9;
  static const int OFFSET_TO_MINUTE = 11;
  static const int OFFSET_TO_SECOND = 13;
  static const int OFFSET_TO_FRACTION = 16;
  static const int OFFSET_TO_OFFSET = 25;
  static const int OFFSET_TO_OFFSET_HOUR = 26;
  static const int OFFSET_TO_OFFSET_MINUTE = 28;
  static const int OFFSET_TO_WEEK = 30;
  static const int OFFSET_TO_DOW = 33;
  static const int OFFSET_TO_DOY  = 34;
  static const int OFFSET_TO_ZZZZ = 37;
  while( source < source_end && dest < dest_end )
    {
    char fchar = *source;
    if( fchar == internal_Y )
      {
      // This can only be a YYYY
      // But, we have a choice.  If there is a 'W' in the format, then we
      // need to use ZZZZ rather than YYYY:
      src = ftime + OFFSET_TO_YYYY;
      const char *p = source;
      while(p < source_end)
        {
        if( *p++ == internal_W )
          {
          src = ftime + OFFSET_TO_ZZZZ;
          }
        }

      ncount = 4;
      }
    else if( fchar == internal_M )
      {
      // This can only be a MM
      ncount = 2;
      src = ftime + OFFSET_TO_MM;
      }
    else if( fchar == internal_D )
      {
      // It can be a D, DD or DDD
      if( source[2] == internal_D )
        {
        ncount = 3;
        src = ftime + OFFSET_TO_DOY;
        }
      else if( source[1] == internal_D )
        {
        ncount = 2;
        src = ftime + OFFSET_TO_DD;
        }
      else
        {
        ncount = 1;
        src = ftime + OFFSET_TO_DOW;
        }
      }
    else if( fchar == internal_plus )
      {
      saw_plus_sign = true;
      ncount = 1;
      src = ftime + OFFSET_TO_OFFSET;
      }
    else if( fchar == internal_h )
      {
      ncount = 2;
      if(saw_plus_sign)
        {
        src = ftime + OFFSET_TO_OFFSET_HOUR;
        }
      else
        {
        src = ftime + OFFSET_TO_HOUR;
        }
      }
    else if( fchar == internal_m )
      {
      ncount = 2;
      if(saw_plus_sign)
        {
        src = ftime + OFFSET_TO_OFFSET_MINUTE;
        }
      else
        {
        src = ftime + OFFSET_TO_MINUTE;
        }
      }
    else if( fchar == decimal_point )
      {
      saw_decimal_point = true;
      ncount = 1;
      src = source;
      }
    else if( fchar == internal_s )
      {
      if(saw_decimal_point)
        {
        // There can be a variable number of fractional 's'
        ncount = -1;
        src = ftime + OFFSET_TO_FRACTION;
        }
      else
        {
        ncount = 2;
        src = ftime + OFFSET_TO_SECOND;
        }
      }
    else if( fchar == internal_W )
      {
      ncount = 3;
      src = ftime + OFFSET_TO_WEEK;
      }
    else
      {
      ncount = 1;
      src = source;
      }

    // Copy over the ncount characters to dest
    if( ncount == -1 )
      {
      // This indicates special processing for a variable number of 's'
      // characters
      while(*source == 's' && dest < dest_end)
        {
        source += 1;
        *dest++ = *src++;
        }
      }
    else
      {
      source += ncount;
      while(ncount-- && dest < dest_end)
        {
        *dest++ = *src++;
        }
      }
    }
  }

//
//
// Beyond this point, we are implementing phase 2 of intrinsics.  These routines
// are intended to be "better" than the ones above.  In an ideal world,
// eventually all of the above routines will migrate down here, and this comment
// will be removed.  Bob Dubner, 2023-01-18

// Although not, of course, necessary, these routines are being placed in
// alphabetical order by the COBOL function name:

extern "C"
void
__gg__abs(cblc_field_t *dest,
    const cblc_field_t *source,
          size_t source_offset,
          size_t source_size)
  {
  // FUNCTION ABS
  GCOB_FP128 value;
  value = __gg__float128_from_qualified_field(source,
                                              source_offset,
                                              source_size);
  if( value < 0 )
    {
    value = -value;
    }
  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__acos( cblc_field_t *dest,
      const cblc_field_t *source,
            size_t        source_offset,
            size_t        source_size)
  {
  // FUNCTION ACOS
  GCOB_FP128 value;
  value = __gg__float128_from_qualified_field(source, source_offset, source_size);

  if( value < GCOB_FP128_LITERAL(-1.00) || value > GCOB_FP128_LITERAL(+1.00) )
    {
    exception_raise(ec_argument_function_e);
    value = WEIRD_TRANSCENDENT_RETURN_VALUE;
    }
  else
    {
    value = FP128_FUNC(acos)(value);
    }

  __gg__float128_to_field( dest,
                           value,
                           truncation_e,
                           NULL);
  }

extern "C"
void
__gg__annuity(cblc_field_t *dest,
        const cblc_field_t *arg1,
              size_t arg1_offset,
              size_t arg1_size,
        const cblc_field_t *arg2,
              size_t arg2_offset,
              size_t arg2_size)
  {
  // FUNCTION ANNUITY

  GCOB_FP128 retval = 0;

  GCOB_FP128 val1 = FP128_FUNC(fabs)(__gg__float128_from_qualified_field(arg1,
                                                                arg1_offset,
                                                                arg1_size));
  GCOB_FP128 val2 = FP128_FUNC(fabs)(__gg__float128_from_qualified_field(arg2,
                                                                arg2_offset,
                                                                arg2_size));
  if( val2 > 0)
    {
    if( val1 < 0 )
      {
      exception_raise(ec_argument_function_e);
      }
    else if( val1 == 0 )
      {
      retval = 1/val2;
      }
    else
      {
      retval = val1 / (1- FP128_FUNC(pow)( (1+val1), -val2 ));
      }
    }
  else
    {
    exception_raise(ec_argument_function_e);
    }
  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__asin( cblc_field_t *dest,
      const cblc_field_t *source,
            size_t source_offset,
            size_t source_size)
  {
  // FUNCTION ASIN

  GCOB_FP128 value;
  value = __gg__float128_from_qualified_field(source,
                                              source_offset,
                                              source_size);

  if( value < GCOB_FP128_LITERAL(-1.0) || value > GCOB_FP128_LITERAL(+1.00) )
    {
    exception_raise(ec_argument_function_e);
    value = WEIRD_TRANSCENDENT_RETURN_VALUE;
    }
  else
    {
    value = FP128_FUNC(asin)(value);
    }

  __gg__float128_to_field( dest,
                           value,
                           truncation_e,
                           NULL);
  }

extern "C"
void
__gg__atan( cblc_field_t *dest,
      const cblc_field_t *source,
            size_t source_offset,
            size_t source_size)
  {
  // FUNCTION ATAN

  GCOB_FP128 value;
  value = __gg__float128_from_qualified_field(source,
                                              source_offset,
                                              source_size);

  value = FP128_FUNC(atan)(value);

  __gg__float128_to_field( dest,
                           value,
                           truncation_e,
                           NULL);
  }

extern "C"
void
__gg__byte_length(cblc_field_t *dest,
            const cblc_field_t */*source*/,
                  size_t /*source_offset*/,
                  size_t source_size)
  {
  // FUNCTION BYTE-LENGTH
  __int128 value = source_size;
  __gg__int128_to_field(dest,
                        value,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__char( cblc_field_t *dest,
      const cblc_field_t *source,
            size_t source_offset,
            size_t source_size)
  {
  int rdigits;

  // The CHAR function takes an integer, the ordinal position.  It
  // returns a single-character string, which is the character at that
  // ordinal position.

  // 'A', with the ascii value of 65, is at the ordinal position 66.

  int ordinal = (int)(__gg__binary_value_from_qualified_field(&rdigits,
                                                              source,
                                                              source_offset,
                                                              source_size));
  ordinal /= __gg__power_of_ten(rdigits);
  int ch = ordinal-1;
  memset(dest->data, internal_space, dest->capacity);
  dest->data[0] = ch;
  }

extern "C"
void
__gg__combined_datetime(cblc_field_t *dest,
                  const cblc_field_t *arg1,
                        size_t arg1_offset,
                        size_t arg1_size,
                  const cblc_field_t *arg2,
                        size_t arg2_offset,
                        size_t arg2_size)
  {
  int rdigits;

  __int128 val1 = (int)(__gg__binary_value_from_qualified_field(&rdigits,
                                                                arg1,
                                                                arg1_offset,
                                                                arg1_size));
  __int128 val2 = (int)(__gg__binary_value_from_qualified_field(&rdigits,
                                                                arg2,
                                                                arg2_offset,
                                                                arg2_size));
  __int128 value = val1 * 1000000 + val2;
  __gg__int128_to_field(dest,
                        value,
                        6,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__concat( cblc_field_t *dest,
              size_t ncount)
  {
  size_t bytes = 0;
  size_t offset = 0;
  for(size_t i=0; i<ncount; i++)
    {
    bytes += __gg__treeplet_1s[i];
    }
  __gg__adjust_dest_size(dest, bytes);
  for(size_t i=0; i<ncount; i++)
    {
    memcpy( dest->data + offset,
            __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i],
            __gg__treeplet_1s[i]);
    offset += __gg__treeplet_1s[i];
    }
  }

extern "C"
void
__gg__cos(cblc_field_t *dest,
     const cblc_field_t *source,
          size_t        source_offset,
          size_t        source_size)
  {
  // FUNCTION COS

  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  value = FP128_FUNC(cos)(value);
  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__current_date(cblc_field_t *dest)
  {
  // FUNCTION CURRENT-DATE
  struct cbl_timespec tp = {};
  __gg__clock_gettime(&tp); // time_t tv_sec; long tv_nsec

  char retval[DATE_STRING_BUFFER_SIZE];
  timespec_to_string(retval, tp);
  ascii_to_internal_str(retval, strlen(retval));
  string_to_dest(dest, retval);
  }

extern "C"
void
__gg__seconds_past_midnight(cblc_field_t *dest)
  {
  // SECONDS-PAST-MIDNIGHT
  struct cbl_timespec tp = {};
  struct tm tm;
  __int128 retval=0;
  
  __gg__clock_gettime(&tp); // time_t tv_sec; long tv_nsec
  localtime_r(&tp.tv_sec, &tm);

  retval += tm.tm_hour;
  retval *= 60;
  retval += tm.tm_min;
  retval *= 60;
  retval += tm.tm_sec;
  retval *= 1000000000;
  retval += tp.tv_nsec;
  __gg__int128_to_field(dest,
                        retval,
                        9,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__date_of_integer(cblc_field_t *dest,
                const cblc_field_t *source,
                      size_t source_offset,
                      size_t source_size)
  {
  // FUNCTION DATE-OF-INTEGER
  int rdigits;
  double JD = (double)__gg__binary_value_from_qualified_field(&rdigits,
                                                              source,
                                                              source_offset,
                                                              source_size);
  JD += JD_OF_1601_01_02;
  int Y;
  int M;
  int D;
  JD_to_YMD(Y, M, D, JD);
  int retval = Y*10000 + M*100 + D;
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__date_to_yyyymmdd( cblc_field_t *dest,
                  const cblc_field_t *par1,
                        size_t par1_o,
                        size_t par1_s,
                  const cblc_field_t *par2,
                        size_t par2_o,
                        size_t par2_s,
                  const cblc_field_t *par3,
                        size_t par3_o,
                        size_t par3_s)
  {
  // FUNCTION DATE-TO-YYYYMMDD
  // See the discussion in ISO/IEC 2014-1989 Section 15.20
  int rdigits;
  int arg1 = (int)__gg__binary_value_from_qualified_field(&rdigits, par1, par1_o, par1_s);
  int arg2 = (int)__gg__binary_value_from_qualified_field(&rdigits, par2, par2_o, par2_s );
  int arg3 = (int)__gg__binary_value_from_qualified_field(&rdigits, par3, par3_o, par3_s);

  int yy   = arg1/10000;
  int mmdd = arg1%10000;

  int retval = year_to_yyyy(yy, arg2, arg3) * 10000 + mmdd;
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__day_of_integer( cblc_field_t *dest,
                const cblc_field_t *source,
                      size_t source_offset,
                      size_t source_size)
  {
  // FUNCTION DAY-OF_INTEGER
  int rdigits;
  double JD = (double)__gg__binary_value_from_qualified_field(&rdigits,
                                                              source,
                                                              source_offset,
                                                              source_size);
  JD += JD_OF_1601_01_02;
  int Y;
  int M;
  int D;
  JD_to_YMD(Y, M, D, JD);

  double start_of_year = YMD_to_JD(Y, 1, 1);

  __int128 retval = Y * 1000 + int(JD - start_of_year) + 1;
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__day_to_yyyyddd( cblc_field_t *dest,
                const cblc_field_t *par1,
                      size_t par1_o,
                      size_t par1_s,
                const cblc_field_t *par2,
                      size_t par2_o,
                      size_t par2_s,
                const cblc_field_t *par3,
                      size_t par3_o,
                      size_t par3_s)
  {
  // FUNCTION DAY-TO-YYYYDDD
  // See the discussion in ISO/IEC 2014-1989 Section 15.20
  int rdigits;
  int arg1 = (int)__gg__binary_value_from_qualified_field(&rdigits, par1, par1_o, par1_s);
  int arg2 = (int)__gg__binary_value_from_qualified_field(&rdigits, par2, par2_o, par2_s );
  int arg3 = (int)__gg__binary_value_from_qualified_field(&rdigits, par3, par3_o, par3_s);

  int yy  = arg1/1000;
  int ddd = arg1%1000;

  int retval = year_to_yyyy(yy, arg2, arg3) * 1000 + ddd;

  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__e(cblc_field_t *dest)
  {
  // FUNCTION E
  static GCOB_FP128 e
    = GCOB_FP128_LITERAL(2.7182818284590452353602874713526624977572);
  __gg__float128_to_field(dest,
                          e,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__exp(cblc_field_t *dest,
    const cblc_field_t *source,
          size_t source_offset,
          size_t source_size)
  {
  // FUNCTION EXP

  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  value = FP128_FUNC(exp)(value);
  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__exp10(cblc_field_t *dest,
      const cblc_field_t *source,
            size_t source_offset,
            size_t source_size)
  {
  // FUNCTION EXP10

  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  value = FP128_FUNC(pow)(GCOB_FP128_LITERAL(10.0), value);
  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__factorial(cblc_field_t *dest,
          const cblc_field_t *source,
                size_t source_offset,
                size_t source_size)
  {
  // FUNCTION FACTORIAL
  int rdigits;
  int N = (int)__gg__binary_value_from_qualified_field( &rdigits,
                                                        source,
                                                        source_offset,
                                                        source_size);
  while(rdigits--)
    {
    N /= 10;
    }

  __int128 retval = 1;

  while( N > 1 )
    {
    retval *= N--;
    }
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__formatted_current_date( cblc_field_t *dest, // Destination string
                         const cblc_field_t *input, // datetime format
                              size_t input_offset,
                              size_t input_size)
  {
  // FUNCTION FORMATTED-CURRENT-DATE

  // Establish the destination, and set it to spaces
  char *d    = PTRCAST(char, dest->data);
  const char *dend = d + dest->capacity;
  memset(d, internal_space, dest->capacity);

  // Establish the formatting string:
  const char *format     = PTRCAST(char, (input->data+input_offset));
  const char *format_end = format + input_size;

  bool is_zulu = false;

  const char *p = format;
  while( p < format_end )
    {
    int ch = *p++;
    if( ch == internal_Z )
      {
      is_zulu = true;
      break;
      }
    }

  struct cbl_timespec ts = {};
  __gg__clock_gettime(&ts);

  struct tm tm = {};
#ifdef HAVE_STRUCT_TM_TM_ZONE
  tm.tm_zone = "GMT";
#endif
  if( is_zulu )
    {
    gmtime_r(&ts.tv_sec, &tm);
    }
  else
    {
    localtime_r(&ts.tv_sec, &tm);
    }

  struct cobol_tm ctm = {};
  populate_ctm_from_tm(ctm, tm);

  ctm.nanoseconds = ts.tv_nsec;

  tzset();
  // Convert seconds west of UTC to minutes east of UTC
  ctm.tz_offset = -timezone/60;

  char achftime[64];
  get_all_time(achftime, ctm);
  ftime_replace(d, dend, format, format_end, achftime);
  }

extern "C"
void
__gg__formatted_date(cblc_field_t *dest, // Destination string
               const cblc_field_t *arg1, // datetime format
                     size_t arg1_offset,
                     size_t arg1_size,
               const cblc_field_t *arg2, // integer date
                     size_t arg2_offset,
                     size_t arg2_size)
  {
  // FUNCTION FORMATTED-DATE

  // Establish the destination, and set it to spaces
  char *d    = PTRCAST(char, dest->data);
  const char *dend = d + dest->capacity;
  memset(d, internal_space, dest->capacity);

  // Establish the formatting string:
  char *format     = PTRCAST(char, (arg1->data+arg1_offset));
  const char *format_end = format + arg1_size;

  struct cobol_tm ctm = {};

  populate_ctm_from_date(ctm, arg2, arg2_offset, arg2_size);

  char achftime[64];
  get_all_time(achftime, ctm);
  if( __gg__exception_code )
    {
    memset(d, internal_space, dend-d);
    }
  else
    {
    ftime_replace(d, dend, format, format_end, achftime);
    __gg__adjust_dest_size(dest, format_end-format);
    }
  }

extern "C"
void
__gg__formatted_datetime( cblc_field_t *dest, // Destination string
                    const cblc_field_t *par1, // datetime format
                          size_t par1_o,
                          size_t par1_s,
                    const cblc_field_t *par2, // integer date
                          size_t par2_o,
                          size_t par2_s,
                    const cblc_field_t *par3, // numeric time
                          size_t par3_o,
                          size_t par3_s,
                    const cblc_field_t *par4, // optional offset in seconds
                          size_t par4_o,
                          size_t par4_s
                          )
  {
  // FUNCTION FORMATTED-DATETIME

  // Establish the destination, and set it to spaces
        char *d    = PTRCAST(char, (dest->data));
  const char *dend = d + dest->capacity;
  memset(d, internal_space, dest->capacity);

  // Establish the formatting string:
  char *format     = PTRCAST(char, (par1->data+par1_o));
  char *format_end = format + par1_s;
  trim_trailing_spaces(format, format_end);
  bool is_zulu = is_zulu_format(format, format_end);

  struct cobol_tm ctm = {};

  populate_ctm_from_date(ctm, par2, par2_o, par2_s);
  populate_ctm_from_time( ctm,
                          par3, par3_o, par3_s,
                          par4, par4_o, par4_s);

  if( is_zulu )
    {
    convert_to_zulu(ctm);
    }

  char achftime[64];
  get_all_time(achftime, ctm);
  if( __gg__exception_code )
    {
    memset(d, internal_space, dend-d);
    }
  else
    {
    ftime_replace(d, dend, format, format_end, achftime);
    __gg__adjust_dest_size(dest, format_end-format);
    }
  }

extern "C"
void
__gg__formatted_time( cblc_field_t *dest,// Destination string
                const cblc_field_t *par1, // datetime format
                      size_t par1_o,
                      size_t par1_s,
                const cblc_field_t *par2,// numeric time
                      size_t par2_o,
                      size_t par2_s,
                const cblc_field_t *par4, // optional offset in seconds
                      size_t par4_o,
                      size_t par4_s)

  {
  // FUNCTION FORMATTED-TIME

  // Establish the destination, and set it to spaces
  char *d          = PTRCAST(char, dest->data);
  const char *dend = d + dest->capacity;
  memset(d, internal_space, dest->capacity);

  // Establish the formatting string:
  char *format     = PTRCAST(char, (par1->data+par1_o));
  char *format_end = format + par1_s;
  trim_trailing_spaces(format, format_end);
  bool is_zulu = is_zulu_format(format, format_end);

  struct cobol_tm ctm = {};
  populate_ctm_from_time( ctm,
                          par2,
                          par2_o,
                          par2_s,
                          par4,
                          par4_o,
                          par4_s);

  if( is_zulu )
    {
    convert_to_zulu(ctm);
    }

  char achftime[64];
  get_all_time(achftime, ctm);
  if( __gg__exception_code )
    {
    memset(d, internal_space, dend-d);
    }
  else
    {
    ftime_replace(d, dend, format, format_end, achftime);
    __gg__adjust_dest_size(dest, format_end-format);
    }
  }

extern "C"
void
__gg__integer(cblc_field_t *dest,
        const cblc_field_t *source,
              size_t source_offset,
              size_t source_size)
  {
  // FUNCTION INTEGER
  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  value = FP128_FUNC(floor)(value);
  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__integer_of_date(cblc_field_t *dest,
                const cblc_field_t *source,
                      size_t source_offset,
                      size_t source_size)
  {
  // FUNCTION INTEGER-OF-DATE
  int rdigits;
  long argument_1 = (long)(__gg__binary_value_from_qualified_field(&rdigits,
                                                              source,
                                                              source_offset,
                                                              source_size));

  int retval = 0;
  static const int max_days[13] = {0, 31, 28, 31, 30, 31, 30,
                                   31, 31, 30, 31, 30, 31};

  int year  = (long)argument_1/10000;
  int month = (long)argument_1/100 % 100;
  int day   = (long)argument_1 % 100;

  // We need to check for validity in the proleptic Gregorian calendar.

  int max_day = 0;
  if( month >= 1 && month <= 12 )
    {
    max_day = max_days[month];
    }
  if( max_day == 28 && (((year%4) == 0 && ((year)%100) != 0) || ((year%400) == 0) ))
    {
    // Year is divisible by four, but is not divisible by 100, so this
    // is a leap year.
    max_day += 1;
    }
  if( day < 1 || day > max_day )
    {
    max_day = 0;
    }
  if( max_day && year >= 1601 && year <= 9999 )
    {
    // It's a valid Y/M/D:
    double JD = YMD_to_JD(year, month, day);

    // Offset result so that 1601-01-01 comes back as the first day of
    // the Gregorian Calendar
    retval = (int)(JD - JD_OF_1601_01_02);
    }
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__integer_of_day( cblc_field_t *dest,
                const cblc_field_t *source,
                      size_t source_offset,
                      size_t source_size)
  {
  // FUNCTION INTEGER-OF-DAY
  // Convert YYYYDDD to "integer date"
  int rdigits;
  int yyyyddd = (int)__gg__binary_value_from_qualified_field( &rdigits,
                                                              source,
                                                              source_offset,
                                                              source_size);
  int yyyy = yyyyddd / 1000;
  int ddd  = yyyyddd % 1000;

  double JD = YMD_to_JD(yyyy, 1, 0) + ddd;
  int retval = (int)(JD - JD_OF_1601_01_02);

  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__integer_part( cblc_field_t *dest,
              const cblc_field_t *source,
                    size_t source_offset,
                    size_t source_size)
  {
  // FUNCTION INTEGER-PART
  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  GCOB_FP128 retval = FP128_FUNC(floor)(FP128_FUNC(fabs)(value));

  if( value < 0 )
    {
    retval = -retval;
    }
  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__fraction_part(cblc_field_t *dest,
              const cblc_field_t *source,
                    size_t source_offset,
                    size_t source_size)
  {
  // FUNCTION INTEGER-PART
  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  bool is_negative = false;
  if( value < 0 )
    {
    is_negative = true;
    value = -value;
    }

  GCOB_FP128 retval = value - FP128_FUNC(floor)(value);

  if( is_negative )
    {
    retval = -retval;
    }
  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__log(cblc_field_t *dest,
    const cblc_field_t *source,
          size_t source_offset,
          size_t source_size)
  {
  // FUNCTION LOG
  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  if( value <= 0.00 )
    {
    exception_raise(ec_argument_function_e);
    }
  else
    {
    GCOB_FP128 retval = FP128_FUNC(log)(value);
    __gg__float128_to_field(dest,
                            retval,
                            truncation_e,
                            NULL);
    }
  }

extern "C"
void
__gg__log10(cblc_field_t *dest,
      const cblc_field_t *source,
            size_t source_offset,
            size_t source_size)
  {
  // FUNCTION LOG10
  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  if( value <= 0.00 )
    {
    exception_raise(ec_argument_function_e);
    }
  else
    {
    GCOB_FP128 retval = FP128_FUNC(log10)(value);
    __gg__float128_to_field(dest,
                            retval,
                            truncation_e,
                            NULL);
    }
  }

extern "C"
void
__gg__max(cblc_field_t *dest,
          size_t ncount)
  {
  // FUNCTION MAX

  if( (    __gg__treeplet_1f[0]->type == FldAlphanumeric
        || __gg__treeplet_1f[0]->type == FldLiteralA) )
    {
    cblc_field_t  *best_field      ;
    unsigned char *best_location = nullptr  ;
    size_t         best_length   = 0        ;
    int            best_attr       ;
    int            best_flags      ;

    bool first_time = true;
    assert(ncount);
    for(size_t i=0; i<ncount; i++)
      {
      refer_state_for_all state;

      build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);

      for(;;)
        {
        if( first_time )
          {
          first_time      = false;
          best_field      = __gg__treeplet_1f[i];
          best_location   = __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i];
          best_length     = __gg__treeplet_1s[i];
          best_attr       = __gg__treeplet_1f[i]->attr;
          best_flags      = __gg__fourplet_flags[i];
          }
        else
          {
          cblc_field_t  *candidate_field      = __gg__treeplet_1f[i];
          unsigned char *candidate_location   = __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i];
          size_t         candidate_length     = __gg__treeplet_1s[i];
          int            candidate_attr       = __gg__treeplet_1f[i]->attr;
          int            candidate_flags      = __gg__fourplet_flags[i];

          int compare_result = __gg__compare_2(
                                 candidate_field,
                                 candidate_location,
                                 candidate_length,
                                 candidate_attr,
                                 candidate_flags,
                                 best_field,
                                 best_location,
                                 best_length,
                                 best_attr,
                                 best_flags,
                                 0);
          if( compare_result >= 0 )
            {
            best_field      = candidate_field      ;
            best_location   = candidate_location   ;
            best_length     = candidate_length     ;
            best_attr       = candidate_attr       ;
            best_flags      = candidate_flags      ;
            }
          }
        if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
          {
          // There is nothing left to do.
          break;
          }
        }
      }


    __gg__adjust_dest_size(dest, best_length);
    dest->type = FldAlphanumeric;
    assert(best_location);
    memcpy(dest->data, best_location, best_length);
    }
  else
    {
    GCOB_FP128 retval;
    bool first_time = true;
    assert(ncount);
    for(size_t i=0; i<ncount; i++)
      {
      refer_state_for_all state;
      build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);

      for(;;)
        {
        if( first_time )
          {
          first_time = false;
          retval = __gg__float128_from_qualified_field(__gg__treeplet_1f[i], __gg__treeplet_1o[i], __gg__treeplet_1s[i]);
          }
        else
          {
          GCOB_FP128 candidate = __gg__float128_from_qualified_field(__gg__treeplet_1f[i], __gg__treeplet_1o[i], __gg__treeplet_1s[i]);
          if( candidate >= retval )
            {
            retval = candidate;
            }
          }
        if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
          {
          // There is nothing left to do for that input.
          break;
          }
        }
      }
    __gg__float128_to_field(dest,
                            retval,
                            truncation_e,
                            NULL);
    }
  }

extern "C"
void
__gg__lower_case( cblc_field_t *dest,
            const cblc_field_t *input,
                  size_t        input_offset,
                  size_t        input_size)
  {
  size_t dest_length = dest->capacity;
  size_t source_length = input_size;
  memset(dest->data, internal_space, dest_length);
  memcpy(dest->data, input->data+input_offset, std::min(dest_length, source_length));
  internal_to_ascii( PTRCAST(char, dest->data), dest_length);
  std::transform(dest->data, dest->data + dest_length, dest->data,
		 [](unsigned char c) { return std::tolower(c); });
  ascii_to_internal_str( PTRCAST(char, dest->data), dest_length);
  }

extern "C"
void
__gg__mean( cblc_field_t *dest,
            size_t ninputs)
  {
  // FUNCTION MEAN
  size_t k_count;
  GCOB_FP128 sum = kahan_summation(ninputs,
                                  __gg__treeplet_1f,
                                  __gg__treeplet_1o,
                                  __gg__treeplet_1s,
                                  __gg__fourplet_flags,
                                  &k_count);
  sum /= k_count;
  __gg__float128_to_field(dest,
                          sum,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__median( cblc_field_t *dest,
              size_t        ncount)
  {
  // FUNCTION MEDIAN

  // This is wasteful, because it allocates N values in order to sort them.  It
  // is also an O(NlogN) solution, when there are O(N) solutions available.

  // It has the merit of being very simple.

  // The future beckons, but not today.

  size_t list_size = 1;

  GCOB_FP128 *the_list = static_cast<GCOB_FP128 *>(malloc(list_size *sizeof(GCOB_FP128)));
  massert(the_list);
  size_t k_count = 0;
  assert(ncount);
  for(size_t i=0; i<ncount; i++)
    {
    refer_state_for_all state;
    build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);

    for(;;)
      {
      if(k_count >= list_size)
        {
        list_size *= 2;
        the_list = PTRCAST(GCOB_FP128, realloc(the_list, list_size *sizeof(GCOB_FP128)));
        massert(the_list);
        }

      assert(the_list);
      the_list[k_count] = __gg__float128_from_qualified_field(__gg__treeplet_1f[i],
                                                              __gg__treeplet_1o[i],
                                                              __gg__treeplet_1s[i]);
      k_count += 1;
      if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
        {
        // There is nothing left to do.
        break;
        }
      }
    }
  std::sort(the_list, the_list+k_count);

  GCOB_FP128 retval;
  size_t i=k_count/2;
  if( k_count & 1 )
    {
    retval = the_list[i];
    }
  else
    {
    retval = (the_list[i-1] + the_list[i])/2.0;
    }
  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  free(the_list);
  }

extern "C"
void
__gg__midrange( cblc_field_t *dest,
                size_t        ncount)
  {
  // FUNCTION MIDRANGE
  GCOB_FP128 val;
  GCOB_FP128 min=0;
  GCOB_FP128 max=0;
  bool first_time = true;
  assert(ncount);
  for(size_t i=0; i<ncount; i++)
    {
    refer_state_for_all state;
    build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);
    for(;;)
      {
      val = __gg__float128_from_qualified_field(__gg__treeplet_1f[i],
                                                __gg__treeplet_1o[i],
                                                __gg__treeplet_1s[i]);
      if( first_time )
        {
        first_time = false;
        min = val;
        max = val;
        }
      min = std::min(min, val);
      max = std::max(max, val);
      if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
        {
        // There is nothing left to do for that input.
        break;
        }
      }
    }
  GCOB_FP128 retval = (min + max)/2.0;
  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__min(cblc_field_t *dest,
          size_t ncount)
  {
  // FUNCTION MIN

  if( (    __gg__treeplet_1f[0]->type == FldAlphanumeric
        || __gg__treeplet_1f[0]->type == FldLiteralA) )
    {
    cblc_field_t  *best_field               ;
    unsigned char *best_location = nullptr  ;
    size_t         best_length   = 0        ;
    int            best_attr                ;
    int            best_flags               ;

    bool first_time = true;
    assert(ncount);
    for(size_t i=0; i<ncount; i++)
      {
      refer_state_for_all state;

      build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);

      for(;;)
        {
        if( first_time )
          {
          first_time      = false;
          best_field      = __gg__treeplet_1f[i];
          best_location   = __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i];
          best_length     = __gg__treeplet_1s[i];
          best_attr       = __gg__treeplet_1f[i]->attr;
          best_flags      = __gg__fourplet_flags[i];
          }
        else
          {
          cblc_field_t  *candidate_field      = __gg__treeplet_1f[i];
          unsigned char *candidate_location   = __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i];
          size_t         candidate_length     = __gg__treeplet_1s[i];
          int            candidate_attr       = __gg__treeplet_1f[i]->attr;
          int            candidate_flags      = __gg__fourplet_flags[i];

          int compare_result = __gg__compare_2(
                                 candidate_field,
                                 candidate_location,
                                 candidate_length,
                                 candidate_attr,
                                 candidate_flags,
                                 best_field,
                                 best_location,
                                 best_length,
                                 best_attr,
                                 best_flags,
                                 0);
          if( compare_result < 0 )
            {
            best_field      = candidate_field      ;
            best_location   = candidate_location   ;
            best_length     = candidate_length     ;
            best_attr       = candidate_attr       ;
            best_flags      = candidate_flags      ;
            }
          }
        if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
          {
          // There is nothing left to do.
          break;
          }
        }
      }

    __gg__adjust_dest_size(dest, best_length);
    dest->type = FldAlphanumeric;
    assert(best_location);
    memcpy(dest->data, best_location, best_length);
    }
  else
    {
    GCOB_FP128 retval;
    bool first_time = true;
    assert(ncount);
    for(size_t i=0; i<ncount; i++)
      {
      refer_state_for_all state;
      build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);

      for(;;)
        {
        if( first_time )
          {
          first_time = false;
          retval = __gg__float128_from_qualified_field(__gg__treeplet_1f[i], __gg__treeplet_1o[i], __gg__treeplet_1s[i]);
          }
        else
          {
          GCOB_FP128 candidate = __gg__float128_from_qualified_field(__gg__treeplet_1f[i], __gg__treeplet_1o[i], __gg__treeplet_1s[i]);
          if( candidate < retval )
            {
            retval = candidate;
            }
          }
        if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
          {
          // There is nothing left to do for that input.
          break;
          }
        }
      }
    __gg__float128_to_field(dest,
                            retval,
                            truncation_e,
                            NULL);
    }
  }

extern "C"
void
__gg__mod(cblc_field_t *dest,
          cblc_field_t *source1,
          size_t source1_offset,
          size_t source1_size,
          cblc_field_t *source2,
          size_t source2_offset,
          size_t source2_size)
  {
  // FUNCTION MOD
  __int128 arg1 = __gg__integer_from_qualified_field(source1,
                                                     source1_offset,
                                                     source1_size);
  __int128 arg2 = __gg__integer_from_qualified_field(source2,
                                                     source2_offset,
                                                     source2_size);
  __int128 retval;

  if( arg2 == 0 )
    {
    exception_raise(ec_argument_function_e);
    retval = 0;
    }
  else
    {
    int sign_of_div  = arg1 >= 0 ? 1 : -1 ;
    sign_of_div     *= arg2 >= 0 ? 1 : -1 ;

    __int128 div = ( arg1 / arg2 ) ;
    if( sign_of_div < 0 )
      {
      div -= 1;
      }

    retval = arg1 - arg2 * div ;
    }

  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

static int
numval( cblc_field_t *dest,
  const cblc_field_t *input,
        size_t input_offset,
        size_t input_size)
  {
  // Returns the one-based character position of a bad character
  // returns zero if it is okay

  const char *p    = PTRCAST(char, (input->data + input_offset));
  const char *pend =     p + input_size;

  int errpos = 0;
  __int128 retval = 0;
  int retval_rdigits = 0;

  bool saw_digit= false;
  char decimal_point = ascii_to_internal(__gg__get_decimal_point());
  bool in_fraction  = false;
  bool leading_sign = false;
  bool is_negative  = false;
  enum
    {
    SPACE1,
    SPACE2,
    DIGITS,
    SPACE3,
    SPACE4,
    } state = SPACE1;

  if( input_size == 0 )
    {
    errpos = 1;
    goto done;
    }
  while( p < pend )
    {
    unsigned char ch = *p++;
    errpos += 1;
    switch( state )
      {
      case SPACE1:
        // We tolerate spaces, and expect to end with a sign, digit,
        // or decimal point:
        if( ch == internal_space )
          {
          continue;
          }
        if( ch == internal_plus )
          {
          leading_sign = true;
          state = SPACE2;
          break;
          }
        if( ch == internal_minus )
          {
          leading_sign = true;
          is_negative  = true;
          state = SPACE2;
          break;
          }
        if( ch >= internal_0 && ch <= internal_9 )
          {
          saw_digit = true;
          retval = ch & 0xF;
          state = DIGITS;
          break;
          }
        if( ch == decimal_point )
          {
          in_fraction = true;
          state = DIGITS;
          break;
          }
        // This is a bad character; errpos is correct
        goto done;
        break;

      case SPACE2:
        // We tolerate spaces, and expect to end with a digit or decimal point:
        if( ch == internal_space )
          {
          break;
          }
        if( ch >= internal_0 && ch <= internal_9 )
          {
          saw_digit = true;
          retval = ch & 0xF;
          state = DIGITS;
          break;
          }
        if( ch == decimal_point )
          {
          in_fraction = true;
          state = DIGITS;
          break;
          }
        // This is a bad character; errpos is correct
        goto done;
        break;

      case DIGITS:
        // We tolerate digits.  We tolerate one decimal point.  We expect to
        // end with a space, a sign, "DB" or "CR", or the the end of the string
        // It's a bit complicated

        if( ch >= internal_0 && ch <= internal_9 )
          {
          saw_digit = true;
          retval *= 10;
          retval += ch & 0xF;
          if( in_fraction )
            {
            retval_rdigits += 1;
            }
          break;
          }
        if( ch == decimal_point && in_fraction )
          {
          // Only one decimal is allowed
          goto done;
          }
        if( ch == decimal_point )
          {
          in_fraction = true;
          break;
          }
        if( ch == internal_space )
          {
          state = SPACE3;
          break;
          }
        if( ch == internal_plus && leading_sign)
          {
          // We are allowed leading or trailing signs, but not both
          goto done;
          }
        if( ch == internal_minus && leading_sign)
          {
          // We are allowed leading or trailing signs, but not both
          goto done;
          }
        if( ch == internal_plus )
          {
          state = SPACE4;
          break;
          }
        if( ch == internal_minus )
          {
          is_negative = true;
          state = SPACE4;
          break;
          }
        if( std::tolower(ch) == 'd' )
          {
          if( leading_sign )
            {
            goto done;
            }
          ch = *p++;
          errpos += 1;
          if( p > pend || std::tolower(ch) != 'b' )
            {
            goto done;
            }
          is_negative = true;
          state = SPACE4;
          break;
          }
        if( std::tolower(ch) == 'c' )
          {
          if( leading_sign )
            {
            goto done;
            }
          ch = *p++;
          errpos += 1;
          if( p > pend || std::tolower(ch) != 'r' )
            {
            goto done;
            }
          is_negative = true;
          state = SPACE4;
          break;
          }
        // This is a bad character; errpos is correct
        goto done;
        break;

      case SPACE3:
        // We tolerate spaces, or we end with a sign:
        if( ch == internal_space )
          {
          break;
          }
        if( ch == internal_plus && leading_sign)
          {
          // We are allowed leading or trailing signs, but not both
          goto done;
          }
        if( ch == internal_minus && leading_sign)
          {
          // We are allowed leading or trailing signs, but not both
          goto done;
          }
        if( ch == internal_plus )
          {
          state = SPACE4;
          break;
          }
        if( ch == internal_minus )
          {
          is_negative = true;
          state = SPACE4;
          break;
          }
        if( std::tolower(ch) == 'd' )
          {
          if( leading_sign )
            {
            goto done;
            }
          ch = *p++;
          errpos += 1;
          if( p > pend || std::tolower(ch) != 'b' )
            {
            goto done;
            }
          is_negative = true;
          state = SPACE4;
          break;
          }
        if( std::tolower(ch) == 'c' )
          {
          if( leading_sign )
            {
            goto done;
            }
          ch = *p++;
          errpos += 1;
          if( p > pend || std::tolower(ch) != 'r' )
            {
            goto done;
            }
          is_negative = true;
          state = SPACE4;
          break;
          }
        goto done;
        break;
      case SPACE4:
        if( ch == internal_space )
          {
          break;
          }
        goto done;
        break;
      }
    }
  if( saw_digit )
    {
    errpos = 0;
    }
  else if( p == pend )
    {
    // If we got to the end without seeing adigit, we need to bump the
    // error pointer:
    errpos += 1;
    }

  done:
  if(errpos)
    {
    retval = 0;
    }
  if( is_negative )
    {
    retval = -retval;
    }
  if(dest)
    {
    __gg__int128_to_field(dest,
                          retval,
                          retval_rdigits,
                          truncation_e,
                          NULL);
    }
  return errpos;
  }

static
int
numval_c( cblc_field_t *dest,
    const cblc_field_t *src,
          size_t        src_offset,
          size_t        src_size,
    const cblc_field_t *crcy,
          size_t        crcy_offset,
          size_t        crcy_size
          )
  {
  size_t errcode = 0;

  char *pstart = PTRCAST(char, (src->data+src_offset));
  char *pend   = pstart + src_size;
  char *p      = pstart;

  GCOB_FP128 retval = 0;
  int sign = 0;
  int rdigits = 0;
  int rdigit_bump = 0;
  unsigned char decimal_point     = ascii_to_internal(__gg__get_decimal_point());
  unsigned char decimal_separator = ascii_to_internal(__gg__get_decimal_separator());

  char *currency_start;
  char *currency_end;
  if( crcy )
    {
    currency_start = PTRCAST(char, (crcy->data+crcy_offset));
    currency_end   = currency_start + crcy_size;
    }
  else
    {
    currency_start = __gg__get_default_currency_string();
    currency_end   = currency_start + strlen(currency_start);
    }
  char *pcurrency = currency_start;
  // Trim off spaces from the currency:
  while( *pcurrency == internal_space && pcurrency < currency_end )
    {
    pcurrency += 1;
    }

  while( *(currency_end-1) == internal_space && currency_end > currency_start )
    {
    currency_end -= 1;
    }

  // We will do this as a state machine:

  enum
    {
    first_space,
    first_sign,
    second_space,
    currency,
    before_digits,
    digits,
    after_digits,
    second_sign,
    final_space,
    } state = first_space;

  while( p < pend )
    {
    unsigned char ch = *p++;
    switch( state )
      {
      case first_space   :
        // Eat up spaces, if any, and then dispatch on the first non-space:
        if( ch != internal_space )
          {
          // ch can now be a plus, a minus, a digit, or the first character
          // of the currency string
          if( ch == internal_plus || ch == internal_minus )
            {
            state = first_sign;
            // Decrement to pointer in order to pick up the character again
            p -= 1;
            }
          else if( ch == *pcurrency )
            {
            state = currency;
            p -= 1;
            }
          else if(  (ch >= internal_0 && ch <= internal_9)
                    || ch == decimal_point )
            {
            state = digits;
            p -= 1;
            }
          else
            {
            // We have a bad character.  Set the errcode to be the position of
            // the bad character, and adjust p to break out of the loop.
            // Set the state so that the default error processing is suppressed
            state = final_space;
            errcode = p - pstart;
            p = pend;
            }
          }
        break;

      case first_sign    :
        // We know the character is a plus or a minus:
        if( ch == internal_plus )
          {
          sign = 1;
          state = second_space;
          }
        else
          {
          sign = -1;
          state = second_space;
          }
        break;

      case second_space :
        // Eat up spaces, if any.  This segment has to end with a currency or
        // a digit:
        if( ch != internal_space )
          {
          if( ch == *pcurrency )
            {
            state = currency;
            p -= 1;
            }
          else if(  (ch >= internal_0 && ch <= internal_9)
                    || ch == decimal_point )
            {
            state = digits;
            p -= 1;
            }
          else
            {
            // We have a bad character.  Set the errcode to be the position of
            // the bad character, and adjust p to break out of the loop.
            state = final_space;
            errcode = p - pstart;
            p = pend;
            }
          }
        break;

      case currency     :
        // At this point, the only valid character is the next character
        // in the currency string:
        if( pcurrency >= currency_end )
          {
          // Hey! Look at us! We got through the whole currency string.
          state = before_digits;
          p -= 1;
          }
        else if( ch == *pcurrency++)
          {
          // We are still marching through the currency
          }
        else
          {
          // We have a bad character:
          errcode = p - pstart;
          state = final_space;
          p = pend;
          }
        break;

      case before_digits :
        // Eat up spaces, if any.  This segment has to end with a digit
        if( ch != internal_space )
          {
          if(  (ch >= internal_0 && ch <= internal_9)
               || ch == decimal_point )
            {
            state = digits;
            p -= 1;
            }
          else
            {
            // We have a bad character.  Set the errcode to be the position of
            // the bad character, and adjust p to break out of the loop.
            state = final_space;
            errcode = p - pstart;
            p = pend;
            }
          }
        break;

      case digits     :
        // The only thing allowed here are digits, decimal points, and
        // decimal separators
        if( ch >= internal_0 && ch <= internal_9 )
          {
          // We have a digit.
          rdigits += rdigit_bump;
          retval *= 10;
          retval += ch & 0x0F;
          }
        else if( ch == decimal_point && rdigit_bump)
          {
          // We have a second decimal_point, which is against the rules
          errcode = p - pstart;
          state = final_space;
          p = pend;
          }
        else if( ch == decimal_separator )
          {
          // Commas are ignored
          }
        else if(  ch == decimal_point )
          {
          rdigit_bump = 1;
          }
        else
          {
          // We have something that isn't a digit or decimal point or decimal
          // separator:
          state = after_digits;
          p -= 1;
          }
        break;

      case after_digits  :
        // after digits, the only valid things are spaces, plus, minus, D, or C
        if( ch != internal_space )
          {
          if(       ch == internal_plus
                 || ch == internal_minus
                 || ch == internal_D
                 || ch == internal_d
                 || ch == internal_C
                 || ch == internal_c )
            {
            state = second_sign;
            p -= 1;
            }
          }
        break;

      case second_sign   :
        if( sign )
          {
          // A second sign isn't allowed
          errcode = p - pstart;
          p = pend;
          }
        if( ch == internal_plus )
          {
          sign = 1;
          }
        else if( ch == internal_minus )
          {
          sign = -1;
          }
        else if(    (ch == internal_D || ch == internal_d)
                    && p < pend
                    && (*p == internal_B || *p == internal_b) )
          {
          sign = -1;
          p += 1;
          }
        else if(    (ch == internal_C || ch == internal_c)
                    && p < pend
                    && (*p == internal_R || *p == internal_r) )
          {
          sign = -1;
          p += 1;
          }
        state = final_space;
        break;

      case final_space   :
        // There should be only spaces until the end
        if( ch == internal_space )
          {
          continue;
          }
        // We have a non-space where there should be only space
        state = final_space;
        errcode = p - pstart;
        p = pend;
        break;
      }
    }
  if( sign == 0 )
    {
    sign = 1;
    }
  retval *= sign;

  if( state != after_digits && state != final_space && state != digits )
    {
    // We broke out of the loop too soon:
    errcode = pend - pstart + 1;
    }

  if( dest )
    {
    retval /= __gg__power_of_ten(rdigits);
    __gg__float128_to_field(dest,
                            retval,
                            truncation_e,
                            NULL);
    }
  return (int)errcode;
  }

extern "C"
void
__gg__numval( cblc_field_t *dest,
        const cblc_field_t *source,
              size_t source_offset,
              size_t source_size)
  {
  int errpos = numval(dest, source, source_offset, source_size);
  if( errpos )
    {
    exception_raise(ec_argument_function_e);
    }
  }

extern "C"
void
__gg__test_numval(cblc_field_t *dest,
            const cblc_field_t *source,
                  size_t source_offset,
                  size_t source_size)
  {
  int retval = numval(NULL,  source, source_offset, source_size);
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__numval_c( cblc_field_t *dest,
              const cblc_field_t *src,
                    size_t        src_offset,
                    size_t        src_size,
              const cblc_field_t *crcy,
                    size_t        crcy_offset,
                    size_t        crcy_size
                )
  {
  numval_c( dest,
            src,
            src_offset,
            src_size,
            crcy,
            crcy_offset,
            crcy_size);
  }

extern "C"
void
__gg__test_numval_c(cblc_field_t *dest,
              const cblc_field_t *src,
                    size_t        src_offset,
                    size_t        src_size,
              const cblc_field_t *crcy,
                    size_t        crcy_offset,
                    size_t        crcy_size
                    )
  {
  int retval = numval_c(NULL,
                        src,
                        src_offset,
                        src_size,
                        crcy,
                        crcy_offset,
                        crcy_size);
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__ord(cblc_field_t *dest,
    const cblc_field_t *input,
          size_t input_offset,
          size_t /*input_size*/)
  {
  // We get our input in internal_character form.
  const char *arg = PTRCAST(char, (input->data + input_offset));

  // The ORD function takes a single-character string and returns the
  // ordinal position of that character.

  // In ASCII  mode, an A is 0x41, so we return 0x42
  // In EBCDIC mode, an A is 0xC1, so we return 0xC2

  size_t retval = (arg[0]&0xFF) + 1;
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__ord_min(cblc_field_t *dest,
              size_t ninputs)
  {
  // Sets dest to the one-based ordinal position of the first occurrence
  // of the biggest element in the list of refs[]

  int retval = -1;
  int running_position = -1;

  cblc_field_t  *best;
  unsigned char *best_location;
  size_t         best_length;
  int            best_attr;
  int            best_flags;

  unsigned char  *candidate_location;
  size_t candidate_length;
  int    candidate_attr;
  int    candidate_flags;

  for( size_t i=0; i<ninputs; i++ )
    {
    refer_state_for_all state;

    build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);
    for(;;)
      {
      running_position += 1;
      if( retval == -1)
        {
        // We have to initialize the comparisons:
        retval          = running_position;
        best            = __gg__treeplet_1f[i];
        best_location   = __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i];
        best_length     = __gg__treeplet_1s[i];
        best_attr       = __gg__treeplet_1f[i]->attr;
        best_flags      = __gg__fourplet_flags[i];
        }
      else
        {
        // We need to save the current adjustments, because __gg__compare
        // is free to modify .location
        candidate_location   = __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i];
        candidate_length     = __gg__treeplet_1s[i];
        candidate_attr       = __gg__treeplet_1f[i]->attr;
        candidate_flags      = __gg__fourplet_flags[i];

        int compare_result =
          __gg__compare_2(
            __gg__treeplet_1f[i],
            candidate_location,
            candidate_length,
            candidate_attr,
            candidate_flags,
            best,
            best_location,
            best_length,
            best_attr,
            best_flags,
            0);
        if( compare_result < 0 )
          {
          retval          = running_position;
          best            = __gg__treeplet_1f[i];
          best_location   = candidate_location;
          best_length     = candidate_length;
          best_attr       = candidate_attr;
          best_flags      = candidate_flags;
          }
        }
      if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
        {
        // There is nothing left to do for that input.
        break;
        }
      }
    }

  retval += 1;
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__ord_max(cblc_field_t *dest,
              size_t ninputs)
  {
  // Sets dest to the one-based ordinal position of the first occurrence
  // of the biggest element in the list of refs[]

  int retval = -1;
  int running_position = -1;

  cblc_field_t  *best;
  unsigned char *best_location;
  size_t         best_length;
  int            best_attr;
  int            best_flags;

  unsigned char  *candidate_location;
  size_t candidate_length;
  int    candidate_attr;
  int    candidate_flags;

  for( size_t i=0; i<ninputs; i++ )
    {
    refer_state_for_all state;

    build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);
    for(;;)
      {
      running_position += 1;
      if( retval == -1)
        {
        // We have to initialize the comparisons:
        retval          = running_position;
        best            = __gg__treeplet_1f[i];
        best_location   = __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i];
        best_length     = __gg__treeplet_1s[i];
        best_attr       = __gg__treeplet_1f[i]->attr;
        best_flags      = __gg__fourplet_flags[i];
        }
      else
        {
        // We need to save the current adjustments, because __gg__compare
        // is free to modify .location
        candidate_location   = __gg__treeplet_1f[i]->data + __gg__treeplet_1o[i];
        candidate_length     = __gg__treeplet_1s[i];
        candidate_attr       = __gg__treeplet_1f[i]->attr;
        candidate_flags      = __gg__fourplet_flags[i];

        int compare_result =
          __gg__compare_2(
            __gg__treeplet_1f[i],
            candidate_location,
            candidate_length,
            candidate_attr,
            candidate_flags,
            best,
            best_location,
            best_length,
            best_attr,
            best_flags,
            0);
        if( compare_result > 0 )
          {
          retval          = running_position;
          best            = __gg__treeplet_1f[i];
          best_location   = candidate_location;
          best_length     = candidate_length;
          best_attr       = candidate_attr;
          best_flags      = candidate_flags;
          }
        }
      if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
        {
        // There is nothing left to do for that input.
        break;
        }
      }
    }

  retval += 1;  // Make the result one-based, as per COBOL specification
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__pi(cblc_field_t *dest)
  {
  // FUNCTION PI

  static GCOB_FP128 pi
    = GCOB_FP128_LITERAL(3.141592653589793238462643383279502884);
  __gg__float128_to_field(dest,
                          pi,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__present_value(cblc_field_t *dest,
                    size_t        ncount)
  {
  GCOB_FP128 discount = 0;;
  GCOB_FP128 denom = 1;

  GCOB_FP128 retval = 0;
  bool first_time = true;
  for(size_t i=0; i<ncount; i++)
    {
    refer_state_for_all state;
    build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);
    for(;;)
      {
      if(first_time)
        {
        first_time = false;
        GCOB_FP128 arg1 = __gg__float128_from_qualified_field(__gg__treeplet_1f[i],
                                                             __gg__treeplet_1o[i],
                                                             __gg__treeplet_1s[i]);
        if( arg1 <= GCOB_FP128_LITERAL(-1.0) )
          {
          exception_raise(ec_argument_function_e);
          break;
          }
        discount = GCOB_FP128_LITERAL(1.0) / (GCOB_FP128_LITERAL(1.0) + arg1);
        }
      else
        {
        GCOB_FP128 arg = __gg__float128_from_qualified_field(__gg__treeplet_1f[i],
                                                            __gg__treeplet_1o[i],
                                                            __gg__treeplet_1s[i]);
        denom *= discount;
        retval += arg * denom;
        }
      if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
        {
        // There is nothing left to do for that input.
        break;
        }
      }
    }
  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__range(cblc_field_t *dest,
            size_t        ncount)
  {
  // FUNCTION RANGE
  bool first_time = true;
  GCOB_FP128 val;
  GCOB_FP128 min;
  GCOB_FP128 max;

  assert(ncount > 0);
  for(size_t i=0; i<ncount; i++)
    {
    refer_state_for_all state;
    build_refer_state_for_all(state, __gg__treeplet_1f[i], __gg__fourplet_flags[i]);
    for(;;)
      {
      val = __gg__float128_from_qualified_field(__gg__treeplet_1f[i],
                                                __gg__treeplet_1o[i],
                                                __gg__treeplet_1s[i]);
      if( first_time )
        {
        first_time = false;
        min = val;
        max = val;
        }
      min = std::min(min, val);
      max = std::max(max, val);
      if( !update_refer_state_for_all(state, __gg__treeplet_1f[i]) )
        {
        // There is nothing left to do.
        break;
        }
      }
    }

  GCOB_FP128 retval = max - min;
  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__rem(cblc_field_t *dest,
     const cblc_field_t *par1,
          size_t par1_offset,
          size_t par1_size,
     const cblc_field_t *par2,
          size_t par2_offset,
          size_t par2_size)
  {
  // FUNCTION REM
  //
  // Note:  REM takes two NUMERICS, not necessarily integers

  // The ISO spec says:
  // ((argument-1) – ((argument-2) * FUNCTION INTEGER-PART ((argument-1) / (argument-2))))

  GCOB_FP128 arg1 = __gg__float128_from_qualified_field( par1,
                                                        par1_offset,
                                                        par1_size);
  GCOB_FP128 arg2 = __gg__float128_from_qualified_field( par2,
                                                        par2_offset,
                                                        par2_size);

  GCOB_FP128 intpart;
  GCOB_FP128 retval;
  if( arg2 == 0 )
    {
    exception_raise(ec_argument_function_e);
    retval = 0;
    }
  else
    {
    FP128_FUNC(modf)(arg1 / arg2, &intpart);
    retval = arg1 - arg2 * intpart;
    }

  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__trim( cblc_field_t *dest,
      const cblc_field_t *arg1,
            size_t        arg1_offset,
            size_t        arg1_size,
      const cblc_field_t *arg2,
            size_t        arg2_offset,
            size_t        arg2_size)
  {
  int rdigits;
  __int128 type = __gg__binary_value_from_qualified_field(&rdigits,
                                                          arg2,
                                                          arg2_offset,
                                                          arg2_size);
  //static const int BOTH     = 0;
  static const int LEADING  = 1;  // Remove leading  spaces
  static const int TRAILING = 2;  // Remove trailing spaces

  if(   dest->type != FldAlphanumeric ||
        !(dest->attr & intermediate_e) )
    {
    fprintf(stderr,
            "We expect the target of a FUNCTION TIME to "
            "be an intermediate alphanumeric\n");
    abort();
    }
  dest->capacity = dest->offset;

  // No matter what, we want to find the leftmost non-space and the
  // rightmost non-space:

  char *left  = PTRCAST(char, (arg1->data+arg1_offset));
  char *right = left + arg1_size-1;

  // Find left and right: the first and last non-spaces
  while( left <= right )
    {
    if( *left != internal_space && *right != internal_space )
      {
      break;
      }
    if( *left == internal_space )
      {
      left += 1;
      }
    if( *right == internal_space )
      {
      right -= 1;
      }
    }
  if( type == LEADING )
    {
    // We want to leave any trailing spaces, so we return 'right' to its
    // original value:
    right = PTRCAST(char, (arg1->data+arg1_offset)) + arg1_size-1;
    }
  else if( type == TRAILING )
    {
    // We want to leave any leading spaces, so we return 'left' to its
    // original value:
    left = PTRCAST(char, (arg1->data+arg1_offset));
    }

  if( left > right )
    {
    // When the arg1 input string was empty, we want left to be right+1.
    // The left/right loop can sometimes end up with left equal to right+2.
    // That needs to be fixed:
    left = right+1;
    }

  size_t ncount = right+1 - left;
  __gg__adjust_dest_size(dest, ncount);

  // Because it's a temporary, we are weakly confident that we can change
  // the capacity to match what we want.  At this writing, we aren't 100%
  // sure of the implications of the run-time capacity not matching what the
  // compiler believes the capacity to be at compile-time.  But we obviously
  // think it'll be okay.

  char *dest_left  = PTRCAST(char, dest->data);
  char *dest_right = dest_left + dest->capacity - 1;
  const char *dest_end   = dest_left + dest->capacity;

  while( dest_left <= dest_right && left <= right )
    {
    *dest_left++ = *left++;
    }
  while(dest_left < dest_end)
    {
    *dest_left++ = internal_space;
    }
  }

#if HAVE_INITSTATE_R && HAVE_SRANDOM_R && HAVE_RANDOM_R
static struct random_data *buf = NULL;
static char *state = NULL;
static const size_t state_len = 256;
#else
static unsigned seed = 0;
#endif

extern "C"
void
__gg__random( cblc_field_t *dest,
        const cblc_field_t *input,
              size_t        input_offset,
              size_t        input_size)
  {
  int32_t retval_31;
  int rdigits;
#if HAVE_INITSTATE_R && HAVE_SRANDOM_R && HAVE_RANDOM_R
  // This creates a thread-safe pseudo-random number generator
  // using input as the seed

  // The return value is between zero and not quite one

  if( !buf )
    {
    // This is the very first time through
    buf = (random_data *)malloc(sizeof(struct random_data));
    buf->state = NULL;
    state = (char *)malloc(state_len);

    struct cbl_timespec ts;
    __gg__clock_gettime(&ts);
    initstate_r( ts.tv_nsec, state, state_len, buf);
    }
  int seed = (int)__gg__binary_value_from_qualified_field(&rdigits,
                                                          input,
                                                          input_offset,
                                                          input_size);
  srandom_r(seed, buf);

  random_r(buf, &retval_31);
#else
  seed = (unsigned)__gg__binary_value_from_qualified_field(&rdigits,
                                                          input,
                                                          input_offset,
                                                          input_size);
  srandom (seed);
  retval_31 = random ();
#endif
  // We are going to convert this to a value between zero and not quite one:
  double retval = double(retval_31) / double(0x80000000UL);
  __gg__double_to_target( dest,
                          retval,
                          truncation_e);
  }

extern "C"
void
__gg__random_next(cblc_field_t *dest)
  {
  int32_t retval_31;
#if HAVE_INITSTATE_R && HAVE_SRANDOM_R && HAVE_RANDOM_R
  // The return value is between zero and not quite one

  if( !buf )
    {
    // This is the very first time through
    buf = (random_data *)malloc(sizeof(struct random_data));
    buf->state = NULL;
    state = (char *)malloc(state_len);
    struct cbl_timespec ts;
    __gg__clock_gettime(&ts);
    initstate_r( ts.tv_nsec, state, state_len, buf);
    }
  random_r(buf, &retval_31);
#else
  retval_31 = random ();
#endif
  // We are going to convert this to a value between zero and not quite one:
  double retval = double(retval_31) / double(0x80000000UL);
  __gg__double_to_target( dest,
                          retval,
                          truncation_e);
  }

extern "C"
void
__gg__reverse(cblc_field_t *dest,
        const cblc_field_t *input,
              size_t input_offset,
              size_t input_size)
  {
  size_t dest_length = dest->capacity;
  size_t source_length = input_size;
  size_t length = std::min(dest_length, source_length);
  memset(dest->data, internal_space, dest_length);
  for(size_t i=0; i<length; i++)
    {
    dest->data[i] = (input->data+input_offset)[source_length-1-i];
    }
  if( (dest->attr & intermediate_e) )
    {
    dest->capacity = std::min(dest_length, source_length);
    }
  }

extern "C"
void
__gg__sign( cblc_field_t *dest,
      const cblc_field_t *source,
            size_t source_offset,
            size_t source_size)
  {
  // FUNCTION SIGN

  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);

  int retval;
  if(value > 0)
    {
    retval = 1;
    }
  else if(value < 0)
    {
    retval = -1;
    }
  else
    {
    retval = 0;
    }
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__sin(cblc_field_t *dest,
    const cblc_field_t *source,
          size_t source_offset,
          size_t source_size)
  {
  // FUNCTION SIN

  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);

  value = FP128_FUNC(sin)(value);

  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__sqrt( cblc_field_t *dest,
      const cblc_field_t *source,
            size_t source_offset,
            size_t source_size)
  {
  // FUNCTION SQRT

  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);

  if( value < GCOB_FP128_LITERAL(0.0) )
    {
    exception_raise(ec_argument_function_e);
    }
  else
    {
    value = FP128_FUNC(sqrt)(value);
    }

  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__standard_deviation( cblc_field_t *dest,
                          size_t        ninputs)
  {
  // FUNCTION STANDARD-DEVIATION
  GCOB_FP128 retval = variance(ninputs,
                              __gg__treeplet_1f,
                              __gg__treeplet_1o,
                              __gg__treeplet_1s,
                              __gg__fourplet_flags);
  retval = FP128_FUNC(sqrt)(retval);

  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__sum(cblc_field_t *dest,
          size_t        ninputs)
  {
  // FUNCTION SUM
  size_t k_count;
  GCOB_FP128 sum = kahan_summation(ninputs,
                                  __gg__treeplet_1f,
                                  __gg__treeplet_1o,
                                  __gg__treeplet_1s,
                                  __gg__fourplet_flags,
                                  &k_count);
  __gg__float128_to_field(dest,
                          sum,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__tan(cblc_field_t *dest,
    const cblc_field_t *source,
          size_t source_offset,
          size_t source_size)
  {
  // FUNCTION TAN

  GCOB_FP128 value = __gg__float128_from_qualified_field(source,
                                                        source_offset,
                                                        source_size);
  value = FP128_FUNC(tan)(value);
  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__test_date_yyyymmdd( cblc_field_t *dest,
                    const cblc_field_t *source,
                          size_t source_offset,
                          size_t source_size)
  {
  int rdigits;
  int yyyymmdd = (int)__gg__binary_value_from_qualified_field(&rdigits,
                                                              source,
                                                              source_offset,
                                                              source_size);
  int retval;
  int mmdd = yyyymmdd % 10000;
  int mm   = mmdd     /   100;
  if( yyyymmdd < 16010000 || yyyymmdd > 99999999 )
    {
    retval = 1;
    }
  else if( mm < 1 || mm > 12 )
    {
    retval = 2;
    }
  else
    {
    int dd   = yyyymmdd %   100;
    int yyyy = yyyymmdd / 10000;
    int jy;
    int jm;
    int jd;
    double JD;

    // If there is something wrong with the number of days per month for a
    // given year, the Julian Date conversion won't reverse properly.
    // For example, January 32 will come back as February 1
    JD = YMD_to_JD(yyyy, mm, dd);
    JD_to_YMD(jy, jm, jd, JD);
    if( jd == dd && jm == mm && jy == yyyy )
      {
      retval = 0;
      }
    else
      {
      retval = 3;
      }
    }
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__test_day_yyyyddd( cblc_field_t *dest,
                  const cblc_field_t *source,
                        size_t source_offset,
                        size_t source_size)
  {
  int rdigits;
  int yyyyddd = (int)__gg__binary_value_from_qualified_field(&rdigits,
                                                              source,
                                                              source_offset,
                                                              source_size);
  int retval;
  int ddd  = yyyyddd % 1000;
  int yyyy = yyyyddd / 1000;
  int days_in_year;

  days_in_year = is_leap_year(yyyy);

  if( yyyyddd < 1601000 || yyyyddd > 9999999 )
    {
    retval = 1;
    }
  else if( ddd < 1 || ddd > days_in_year)
    {
    retval = 2;
    }
  else
    {
    retval = 0;
    }
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__upper_case( cblc_field_t *dest,
            const cblc_field_t *input,
                  size_t        input_offset,
                  size_t        input_size)
  {
  size_t dest_length = dest->capacity;
  size_t source_length = input_size;
  memset(dest->data, internal_space, dest_length);
  memcpy(dest->data, input->data+input_offset, std::min(dest_length, source_length));
  internal_to_ascii( PTRCAST(char, dest->data), dest_length);
  std::transform(dest->data, dest->data + dest_length, dest->data,
		 [](unsigned char c) { return std::toupper(c); });
  ascii_to_internal_str( PTRCAST(char, dest->data), dest_length);
  }

extern "C"
void
__gg__variance( cblc_field_t *dest,
                size_t        ncount)
  {
  // FUNCTION VARIANCE
  GCOB_FP128 retval = variance(ncount,
                              __gg__treeplet_1f,
                              __gg__treeplet_1o,
                              __gg__treeplet_1s,
                              __gg__fourplet_flags);
  __gg__float128_to_field(dest,
                          retval,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__when_compiled(cblc_field_t *dest, size_t tv_sec, long tv_nsec)
  {
  struct cbl_timespec tp = {};
  tp.tv_sec  = tv_sec;
  tp.tv_nsec = tv_nsec;
  char retval[DATE_STRING_BUFFER_SIZE];
  timespec_to_string(retval, tp);
  ascii_to_internal_str(retval, strlen(retval));
  string_to_dest(dest, retval);
  }

extern "C"
void
__gg__year_to_yyyy( cblc_field_t *dest,
              const cblc_field_t *par1,
                    size_t par1_o,
                    size_t par1_s,
              const cblc_field_t *par2,
                    size_t par2_o,
                    size_t par2_s,
              const cblc_field_t *par3,
                    size_t par3_o,
                    size_t par3_s)
  {
  // FUNCTION YEAR_TO_YYYY
  int rdigits;
  int yy   = (int)__gg__binary_value_from_qualified_field(&rdigits, par1, par1_o, par1_s);
  int arg2 = (int)__gg__binary_value_from_qualified_field(&rdigits, par2, par2_o, par2_s );
  int arg3 = (int)__gg__binary_value_from_qualified_field(&rdigits, par3, par3_o, par3_s);

  int retval = year_to_yyyy(yy, arg2, arg3);

  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

static
int
gets_int(int ndigits, const char *p, const char *pend, int *digits)
  {
  // This routine returns the value of the integer at p.  If there is something
  // wrong with the integer, it returns a negative number, the value being the
  // position (starting at 1) where the problem is.
  int retval = 0;
  memset(digits, 0xFF, ndigits * sizeof(int));
  for(int i=1; i<=ndigits; i++)
    {
    if(p >= pend)
      {
      // We ran out of input too soon
      retval = -i;
      break;
      }
    int ch = *p++;
    if( ch < internal_0 || ch > internal_9 )
      {
      // This isn't a digit zero through nine
      retval = -i;
      break;
      }
    retval *= 10;
    retval += ch & 0xF;
    digits[i-1] = ch & 0xF;
    }
  return retval;
  }

static
int
gets_year(const char *p, const char *pend, struct cobol_tm &ctm)
  {
  // Populates ctm.YYYY, ctm.days_in_year, and ctm.weeks_in_year, which are
  // all determined by the YYYY value.

  // Returns 0 if successful, and returns the ordinal position of the character
  // where a four-character range with a year value of 1601 became impossible.

  int retval = 0;
  int digits[4];
  int YYYY = gets_int(4, p, pend, digits);

  if( digits[0] == -1 || digits[0] == 0 )
    {
    return 1;
    }
  if( digits[1] == -1 )
    {
    return 2;
    }
  if( digits[2] == -1 )
    {
    return 3;
    }
  if( digits[3] == -1 )
    {
    return 4;
    }

  if( YYYY >= 0 )
    {
    // The year has to be > 1000
    if( YYYY < 1000 )
      {
      // We fail on the initial zero
      retval = 1;
      }
    else if( YYYY < 1600 )
      {
      // We fail on the second digit
      retval = 2;
      }
    else if( YYYY == 1600 )
      {
      // We fail on the fourth digit
      retval = 4;
      }
    else
      {
      // The year is a good value
      ctm.YYYY = YYYY;
      ctm.days_in_year = is_leap_year(YYYY);
      ctm.weeks_in_year = weeks_in_year(YYYY);
      }
    }
  else
    {
    retval = -YYYY;
    }
  return retval;
  }

static
int
gets_month(const char *p, const char *pend, struct cobol_tm &ctm)
  {
  // Populates ctm.MM

  // Returns either zero, or else the ordinal position of where the input
  // processing failed.

  int digits[2];
  int retval = 0;
  int MM = gets_int(2, p, pend, digits);

  if( digits[0] == -1 || digits[0] > 1)
    {
    return 1;
    }
  if( digits[1] == -1 )
    {
    return 2;
    }
  if( MM >= 0 )
    {
    if( MM == 0 )
      {
      // We know the month was wrong at the second zero
      retval = 2;
      }
    if( MM >= 20 )
      {
      // We know the month was wrong at the first digit
      retval = 1;
      }
    else if( MM > 12 )
      {
      // We are betweem 13 and 19, so it was the second digit
      retval = 2;
      }
    ctm.MM = MM;
    }
  else
    {
    retval = -MM;
    }
  return retval;
  }

static
int
gets_day(const char *p, const char *pend, struct cobol_tm &ctm)
  {
  // Populates ctm.DD, ctm.day_of_week, ctm.week_of_year, ctm.day_of_week

  // The assumption is that YYYY and MM were populated before arriving here

  int digits[2];
  int retval = 0;
  int DD = gets_int(2, p, pend, digits);

  if( digits[0] == -1 || digits[0] > 3)
    {
    return 1;
    }
  if( digits[1] == -1 )
    {
    return 2;
    }
  if( DD >= 0 )
    {
    if( DD == 0)
      {
      // If zero, we know we failed at the second '0' in "00"
      retval = 2;
      }
    else if( DD >= 40)
      {
      // 40 or more, then we knew there was trouble at the first digit
      retval = 1;
      }
    else if(ctm.MM == 2 && DD >=30)
      {
      // It's February, so if we see 3x we know on the 3 that we are in
      // error:
      retval = 1;
      }
    else
      {
      static const int month_days[13] = {-1,31,28,31,30,31,30,31,31,30,31,30,31};
      int days_in_month = month_days[ctm.MM];
      if( ctm.MM == 2 && ctm.days_in_year == 366 )
        {
        days_in_month = 29;
        }

      if( DD > days_in_month )
        {
        retval = 2;
        }
      else
        {
        // We have a good YYYY-MM-DD
        ctm.DD = DD;
        double JD      = YMD_to_JD(ctm.YYYY, ctm.MM, DD);
        double JD_Jan0 = YMD_to_JD(ctm.YYYY, 1, 0);
        ctm.day_of_year = (int)(JD - JD_Jan0);
        ctm.day_of_week = JD_to_DOW(JD);
        }
      }
    }
  else
    {
    retval = -DD;
    }
  return retval;
  }

static
int
gets_day_of_week(const char *p, const char *pend, struct cobol_tm &ctm)
  {
  // This is just a simple D, for day-of-week.  The COBOL spec is that
  // it be 1 to 7, 1 being Monday
  int digits[1];
  int day_of_week = gets_int(1, p, pend, digits);
  if( day_of_week<0 || day_of_week >7)
    {
    // The single character at source is no good:
    return 1;
    }
  ctm.day_of_week = day_of_week;

  // It is a value 1 through 7.  Convert it to 1 through 6:
  day_of_week -= 1;

  // Find the day-of-year using COBOL week logic:
  double JD_Jan4 = YMD_to_JD(ctm.YYYY, 1, 4);
  double JD_Jan0 = JD_Jan4 - 4;
  int dow_Jan4 = JD_to_DOW(JD_Jan4);
  double week_zero = JD_Jan4 - dow_Jan4;
  double JD = week_zero + (ctm.week_of_year-1)*7 + day_of_week;

  int day_of_year = (int)(JD - JD_Jan0);

  // It's possible for the year/week/day_of_week to be
  // before Jan 1.  This is the case for 1900-12-31, as one example; that
  // date gets converted to 1901-W01-01
  if( day_of_year <= 0 )
    {
    double JD_prior_year = YMD_to_JD(ctm.YYYY-1, 1, 0);
    int    day_of_prior_year = (int)(JD-JD_prior_year);
    int    days_in_prior_year = is_leap_year(ctm.YYYY-1);
    if( day_of_prior_year > days_in_prior_year )
      {
      return 1;
      }
    ctm.ZZZZ = ctm.YYYY + 1;
    day_of_year = day_of_prior_year;
    }

  // Arriving here means we have a good JD, which means we can decompose it
  JD_to_YMD(ctm.YYYY, ctm.MM, ctm.DD, JD);
  ctm.day_of_year = day_of_year;
  return 0;
  }

static
int
gets_day_of_year(const char *p, const char *pend, struct cobol_tm &ctm)
  {
  // This is a three-digit day-of-year, 001 through 365,366
  int digits[3];
  int DDD = gets_int(3, p, pend, digits);
  if( digits[0] == -1 || digits[0] > 3)
    {
    return 1;
    }
  if( digits[1] == -1 )
    {
    return 2;
    }
  if( digits[2] == -1 )
    {
    return 3;
    }
  if( DDD < 0 )
    {
    return -DDD;
    }

  if( DDD == 0 )
    {
    // We know we went wrong at the third '0' in "000"
    return 3;
    }
  if( DDD >= 400 )
    {
    // We know we went wrong at the first digit
    return 1;
    }
  if( DDD >= 370 )
    {
    // We know we went wrong at the second digit
    return 2;
    }
  if( DDD > ctm.days_in_year )
    {
    // We know we went wrong at the third digit
    return 3;
    }
  // We know that DDD is a good value between 1 and ctm.days_in_year
  ctm.day_of_year = DDD;

  double JD_Jan0 = YMD_to_JD(ctm.YYYY, 1, 0);
  double JD = JD_Jan0 + DDD;
  JD_to_YMD(ctm.YYYY,
            ctm.MM,
            ctm.DD,
            JD);
  ctm.day_of_week = JD_to_DOW(JD);
  return 0;
  }

static
int
gets_week(const char *p, const char *pend, struct cobol_tm &ctm)
  {
  // This is a two-digit value, 01 through 52,53
  int digits[2];
  int ww = gets_int(2, p, pend, digits);
  if( digits[0] == -1 || digits[0] > 5 )
    {
    return 1;
    }
  if( digits[1] == -1 )
    {
    return 2;
    }
  if( ww < 0 )
    {
    return -ww;
    }

  if( ww == 0 )
    {
    // We know we went wrong at the second '0' in "00"
    return 2;
    }
  if( ww >= 60 )
    {
    // We know we went wrong at the first digit
    return 1;
    }
  if( ww > ctm.weeks_in_year )
    {
    // We know we went wrong at the second digit
    return 2;
    }
  // We know that ww is a good value for this year.
  ctm.week_of_year = ww;
  return 0;
  }

static
int
gets_hours( const char *p,
            const char *pend,
            struct cobol_tm &ctm,
            bool in_offset)
  {
  // This is a two-digit value, 01 through 23
  int digits[2];
  int hh = gets_int(2, p, pend, digits);

  if( digits[0] == -1 || digits[0] > 2 )
    {
    return 1;
    }
  if( digits[1] == -1 )
    {
    return 2;
    }

  if( hh < 0 )
    {
    return -hh;
    }

  if( hh >= 30 )
    {
    // We know we went wrong at the first digit
    return 1;
    }

  if( hh >= 24 )
    {
    // We know we went wrong at the first digit
    return 2;
    }

  if( in_offset )
    {
    ctm.tz_offset = 60*hh;
    }
  else
    {
    ctm.hh = hh;
    }
  return 0;
  }

static
int
gets_minutes( const char *p,
              const char *pend,
              struct cobol_tm &ctm,
              bool in_offset)
  {
  // This is a two-digit value, 01 through 59
  int digits[2];
  int mm = gets_int(2, p, pend, digits);
  if( digits[0] == -1 || digits[0] > 5 )
    {
    return 1;
    }
  if( digits[1] == -1 )
    {
    return 2;
    }

  if( mm < 0 )
    {
    return -mm;
    }

  if( mm >= 60 )
    {
    // We know we went wrong at the first digit
    return 1;
    }

  if( in_offset )
    {
    ctm.tz_offset += mm;
    }
  else
    {
    ctm.mm = mm;
    }
  return 0;
  }

static
int
gets_seconds(const char *p, const char *pend, struct cobol_tm &ctm)
  {
  // This is a two-digit value, 01 through 59
  int digits[2];
  int ss = gets_int(2, p, pend, digits);
  if( digits[0] == -1 || digits[0] > 5 )
    {
    return 1;
    }
  if( digits[1] == -1 )
    {
    return 2;
    }
  if( ss < 0 )
    {
    return -ss;
    }

  if( ss >= 60 )
    {
    // We know we went wrong at the first digit
    return 1;
    }

  ctm.ss = ss;
  return 0;
  }

static
int
gets_nanoseconds( const char *f,
                  const char *f_end,
                  const char *p,
                  const char *pend,
                  struct cobol_tm &ctm)
  {
  // Because nanoseconds digits to the right of the decimal point can vary from
  // one digit to our implementation-specific limit of nine characters, this
  // routine is slightly different.  If there is an error, that causes a
  // positive return value.  A negative return value contains the number of
  // digits we processed

  int errpos = 0;
  int ncount = 0;
  int nanoseconds = 0;

  const char *pinit = p;
  while( f < f_end && *f == internal_s && p < pend )
    {
    f += 1;
    int ch = *p++;
    errpos += 1;

    if( ch < internal_0 || ch > internal_9 )
      {
      // Let our caller know we see a bad character
      return errpos;
      }

    if(ncount < 9)
      {
      nanoseconds *= 10;
      nanoseconds += ch & 0x0F;
      }
    ncount += 1;
    }
  while(ncount++ < 9)
    {
    nanoseconds *= 10;
    }
  ctm.nanoseconds = nanoseconds;

  return -((int)(p - pinit));
  }

static
int
fill_cobol_tm(cobol_tm &ctm,
        const cblc_field_t *par1,
              size_t par1_offset,
              size_t par1_size,
        const cblc_field_t *par2,
              size_t par2_offset,
              size_t par2_size)
  {
  // Establish the formatting string:
  char *format     = PTRCAST(char, (par1->data+par1_offset));
  char *format_end = format + par1_size;

  // Establish the string to be checked:
  char *source     = PTRCAST(char, (par2->data+par2_offset));
  char *source_end = source + par2_size;

  // Let's eliminate trailing spaces...
  trim_trailing_spaces(format, format_end);
  trim_trailing_spaces(source, source_end);

  bool in_offset = false;
  bool in_nanoseconds = false;

  char decimal_point = __gg__get_decimal_point();

  // We keep constant track of the current error location.
  int retval = 1;
  int errpos;

  // At this juncture, we expect both the format and the source to have valid
  // data.  If they don't, it's because the source is too short, and thus
  // retval is the failure point.
  int bump;
  while( format < format_end && source < source_end )
    {
    char ch = *format;

    if(    ch == internal_T
           || ch == internal_colon
           || ch == internal_minus
           || ch == internal_W)
      {
      // These are just formatting characters.  They need to be duplicated,
      // but are otherwise ignored.
      if( *source != ch )
        {
        break;
        }
      bump = 1;
      goto proceed;
      }

    if( ch == internal_plus )
      {
      // This flags a following hhmm offset.  It needs to match a '+' or '-'
      if(    *source != internal_plus
          && *source != internal_minus
          && *source != internal_zero)
        {
        break;
        }
      if( *source == internal_zero )
        {
        // The next four characters have to be zeroes
        if( source[1] != internal_zero )
          {
          retval += 1;
          break;
          }
        if( source[2] != internal_zero )
          {
          retval += 2;
          break;
          }
        if( source[3] != internal_zero )
          {
          retval += 3;
          break;
          }
        if( source[4] != internal_zero )
          {
          retval += 4;
          break;
          }
        }

      in_offset = true;
      bump = 1;
      goto proceed;
      }

    if( ch == decimal_point )
      {
      // This indicates we are starting to process fractional seconds
      if( *source != decimal_point )
        {
        break;
        }
      in_nanoseconds = true;
      bump = 1;
      goto proceed;
      }

    if( ch == internal_Y )
      {
      errpos = gets_year(source, source_end, ctm);
      if( errpos > 0 )
        {
        retval += errpos - 1;
        break;
        }
      bump = 4;
      goto proceed;
      }

    if( ch == internal_M )
      {
      errpos = gets_month(source, source_end, ctm);
      if( errpos > 0 )
        {
        retval += errpos - 1;
        break;
        }
      bump = 2;
      goto proceed;
      }

    if( ch == internal_D )
      {
      // We have three possibilities: DDD, DD, and D
      if( format[1] != internal_D )
        {
        // A singleton 'D' is a day-of-week
        errpos = gets_day_of_week(source, source_end, ctm);
        if( errpos > 0)
          {
          retval += errpos - 1;
          break;
          }
        bump = 1;
        }
      else if( format[2] != internal_D )
        {
        // This is DD, for day-of-month
        errpos = gets_day(source, source_end, ctm);
        if( errpos > 0)
          {
          retval += errpos - 1;
          break;
          }
        bump = 2;
        }
      else
        {
        // Arriving here means that it is DDD, for day-of-year
        // This is DD, for day-of-month
        errpos = gets_day_of_year(source, source_end, ctm);
        if( errpos > 0)
          {
          retval += errpos - 1;
          break;
          }
        bump = 3;
        }
      goto proceed;
      }

    if( ch == internal_w )
      {
      errpos = gets_week(source, source_end, ctm);
      if( errpos > 0 )
        {
        retval += errpos - 1;
        break;
        }
      bump = 2;
      goto proceed;
      }

    if( ch == internal_h )
      {
      errpos = gets_hours(source, source_end, ctm, in_offset);
      if( errpos > 0 )
        {
        retval += errpos - 1;
        break;
        }
      bump = 2;
      goto proceed;
      }

    if( ch == internal_m )
      {
      errpos = gets_minutes(source, source_end, ctm, in_offset);
      if( errpos > 0 )
        {
        retval += errpos - 1;
        break;
        }
      bump = 2;
      goto proceed;
      }

    if( ch == internal_s && !in_nanoseconds )
      {
      errpos = gets_seconds(source, source_end, ctm);
      if( errpos > 0 )
        {
        retval += errpos - 1;
        break;
        }
      bump = 2;
      goto proceed;
      }

    if( ch == internal_s && in_nanoseconds )
      {
      // Peel off digits to the right of the decimal point one at a time
      errpos = gets_nanoseconds(format, format_end, source, source_end, ctm);
      if( errpos > 0 )
        {
        retval += errpos - 1;
        break;
        }
      bump = -errpos;
      goto proceed;
      }

    if( ch == internal_Z || ch == internal_z )
      {
      // This has to be the end of the road
      if( std::toupper((unsigned char)source[0]) != 'Z' )
        {
        retval += 0;
        break;
        }

      convert_to_zulu(ctm);
      bump = 1;
      goto proceed;
      }

    assert(false);

proceed:
    retval += bump;
    format += bump;
    source += bump;
    }

  if( format >= format_end && source >= source_end)
    {
    // This means we processed the entire format string without seeing an error
    retval = 0;

    // Otherwise, either the format or source was too short
    }
  return retval;
  }

extern "C"
void
__gg__test_formatted_datetime(cblc_field_t *dest,
                        const cblc_field_t *arg1,
                              size_t arg1_offset,
                              size_t arg1_size,
                        const cblc_field_t *arg2,
                              size_t arg2_offset,
                              size_t arg2_size)

  {
  struct cobol_tm ctm = {};

  int retval = fill_cobol_tm( ctm,
                              arg1, arg1_offset, arg1_size,
                              arg2, arg2_offset, arg2_size);
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__integer_of_formatted_date(cblc_field_t *dest,
                          const cblc_field_t *arg1,
                                size_t arg1_offset,
                                size_t arg1_size,
                          const cblc_field_t *arg2,
                                size_t arg2_offset,
                                size_t arg2_size)
  {
  struct cobol_tm ctm = {};

  int retval = fill_cobol_tm( ctm,
                              arg1, arg1_offset, arg1_size,
                              arg2, arg2_offset, arg2_size);
  if(retval)
    {
    retval = 0; // Indicates there was a problem with the input data
    }
  else
    {
    double JD = YMD_to_JD(ctm.YYYY, ctm.MM, ctm.DD);

    // Offset result so that 1601-01-01 comes back as the first day of
    // the Gregorian Calendar
    retval = (int)(JD - JD_OF_1601_01_02);
    }

  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__seconds_from_formatted_time(cblc_field_t *dest,
                            const cblc_field_t *arg1,
                                  size_t arg1_offset,
                                  size_t arg1_size,
                            const cblc_field_t *arg2,
                                  size_t arg2_offset,
                                  size_t arg2_size)
  {
  struct cobol_tm ctm = {};

  double retval = fill_cobol_tm( ctm,
                                 arg1, arg1_offset, arg1_size,
                                 arg2, arg2_offset, arg2_size);
  if(retval > 0)
    {
    retval = 0; // Indicates there was a problem with the input data
    }
  else
    {
    retval = (double)(ctm.hh * 3600 + ctm.mm * 60 + ctm.ss) + ctm.nanoseconds/1000000000.;
    }
  __gg__double_to_target( dest,
                          retval,
                          truncation_e);
  }

extern "C"
void
__gg__hex_of(cblc_field_t *dest,
       const cblc_field_t *field,
             size_t field_offset,
             size_t field_size)
  {
  static const char hex[17] = "0123456789ABCDEF";
  size_t bytes = field_size;
  __gg__adjust_dest_size(dest, 2*bytes);
  for(size_t i=0; i<bytes; i++)
    {
    unsigned char byte = (field->data+field_offset)[i];
    dest->data[2*i] = ascii_to_internal(hex[byte>>4]);
    dest->data[2*i+1] = ascii_to_internal(hex[byte&0xF]);
    }
  }

extern "C"
void
__gg__highest_algebraic(cblc_field_t *dest,
                  const cblc_field_t *var,
                        size_t,
                        size_t)
  {
  __int128 result = 0;
  __int128 result_rdigits = 0;

  if( var->attr & scaled_e )
    {
    result = __gg__power_of_ten(var->digits) - 1;
    if( var->rdigits<0 )
      {
      result *= __gg__power_of_ten(-var->rdigits);
      }
    else
      {
      result_rdigits = var->digits + var->rdigits;
      }
    }
  else if( var->digits == 0 )
    {
    result = (1<<(var->capacity*8)) -1 ;
    if( var->attr & signable_e )
      {
      result >>=1 ;
      }
    }
  else
    {
    result_rdigits = var->rdigits;
    result = __gg__power_of_ten(var->digits) - 1;
    }
  __gg__int128_to_field(dest,
                        result,
                        result_rdigits,
                        truncation_e,
                        NULL);
  }

extern "C"
void
__gg__lowest_algebraic( cblc_field_t *dest,
                  const cblc_field_t *var,
                        size_t,
                        size_t)
  {
  __int128 result = 0;
  __int128 result_rdigits = 0;

  if( var->attr & scaled_e )
    {
    result = __gg__power_of_ten(var->digits) - 1;
    if( var->rdigits<0 )
      {
      result *= __gg__power_of_ten(-var->rdigits);
      }
    else
      {
      result_rdigits = var->digits + var->rdigits;
      }
    if( var->attr & signable_e )
      {
      result = -result;
      }
    else
      {
      result = 0;
      }
    }
  else if( var->digits == 0 )
    {
    result = (1<<(var->capacity*8)) -1 ;
    if( var->attr & signable_e )
      {
      result >>=1 ;
      result += 1;
      result = -result;
      }
    else
      {
      result = 0;
      }
    }
  else
    {
    result_rdigits = var->rdigits;
    result = __gg__power_of_ten(var->digits) - 1;
    if( var->attr & signable_e )
      {
      result = -result;
      }
    else
      {
      result = 0;
      }
    }
  __gg__int128_to_field(dest,
                        result,
                        result_rdigits,
                        truncation_e,
                        NULL);
  }

static int
floating_format_tester(char const * const f, char const * const f_end)
  {
  int retval = -1;
  char decimal_point = __gg__get_decimal_point();

  enum
    {
    SPACE1,
    SPACE2,
    DIGITS1,
    DIGITS2,
    SPACE3,
    SPACE4,
    SPACE5,
    DIGITS3,
    SPACE6,
    } state = SPACE1;
  ssize_t index = 0;
  while(index < f_end - f)
    {
    char ch = f[index];
    switch(state)
      {
      case SPACE1:
        if( ch == internal_space )
          {
          // Just keep looking
          break;
          }
        if(    ch == internal_minus
            || ch == internal_plus)
          {
          state = SPACE2;
          break;
          }
        if( ch >= internal_0 && ch <= internal_9 )
          {
          state = DIGITS1;
          break;
          }
        if( decimal_point )
          {
          state = DIGITS2;
          break;
          }
        // Disallowed character
        retval = index;
        break;

      case SPACE2:
        if( ch == internal_space )
          {
          break;
          }
        if( ch >= internal_0 && ch <= internal_9 )
          {
          state = DIGITS1;
          break;
          }
        if( ch == decimal_point )
          {
          state = DIGITS2;
          break;
          }
        retval = index;
        break;

      case DIGITS1:
        if( ch >= internal_0 && ch <= internal_9 )
          {
          break;
          }
        if( ch == decimal_point )
          {
          state = DIGITS2;
          break;
          }
        if( ch == internal_space )
          {
          state = SPACE3;
          break;
          }
        retval = index;
        break;

      case DIGITS2:
        if( ch >= internal_0 && ch <= internal_9 )
          {
          break;
          }
        if( ch == internal_space )
          {
          state = SPACE3;
          break;
          }
        if( ch == internal_E || ch == internal_e )
          {
          state = SPACE4;
          break;
          }
        retval = index;
        break;

      case SPACE3:
        if( ch == internal_space )
          {
          break;
          }
        if( ch >= internal_0 && ch <= internal_9 )
          {
          retval = index;
          break;
          }
        if( ch == internal_E || ch == internal_e )
          {
          state = SPACE4;
          break;
          }
        retval = index;
        break;

      case SPACE4:
        if( ch == internal_space )
          {
          break;
          }
        if( ch == internal_minus || ch == internal_plus )
          {
          state = SPACE5;
          break;
          }
        if( ch >= internal_0 && ch <= internal_9 )
          {
          state = DIGITS3;
          break;
          }
        retval = index;
        break;

      case SPACE5:
        if( ch == internal_space )
          {
          break;
          }
        if( ch >= internal_0 && ch <= internal_9 )
          {
          state = DIGITS3;
          break;
          }
        retval = index;
        break;

      case DIGITS3:
        if( ch >= internal_0 && ch <= internal_9 )
          {
          break;
          }
        if( ch == internal_space )
          {
          state = SPACE6;
          break;
          }
        retval = index;
        break;

      case SPACE6:
      if( ch == internal_space )
        {
        break;
        }
      retval = index;
      break;
      }

    if( retval > -1 )
      {
      break;
      }
    index += 1;
    }

  retval += 1;
  return retval;
  }

extern "C"
void
__gg__numval_f( cblc_field_t *dest,
          const cblc_field_t *source,
                size_t source_offset,
                size_t source_size)
  {
  GCOB_FP128 value = 0;
  const char *data     = PTRCAST(char, (source->data + source_offset));
  const char *data_end = data + source_size;

  int error = floating_format_tester(data, data_end);

  if( error || source_size >= 256 )
    {
    exception_raise(ec_argument_function_e);
    }
  else
    {
    // Get rid of any spaces in the string
    char ach[256];
    char *p = ach;
    while( data < data_end )
      {
      char ch = *data++;
      if( ch != internal_space )
        {
        *p++ = ch;
        }
      }
    *p++ = '\0';
    value = strtofp128(ach, NULL);
    }
  __gg__float128_to_field(dest,
                          value,
                          truncation_e,
                          NULL);
  }

extern "C"
void
__gg__test_numval_f(cblc_field_t *dest,
              const cblc_field_t *source,
                    size_t source_offset,
                    size_t source_size)
  {
  const char *data     = PTRCAST(char, (source->data + source_offset));
  const char *data_end = data + source_size;

  int error = floating_format_tester(data, data_end);

  __gg__int128_to_field(dest,
                        error,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

static bool
ismatch(const char *a1, const char *a2, const char *b1, const char *b2)
  {
  bool retval = true;
  while( a1 < a2 && b1 < b2 )
    {
    if( *a1++ != *b1++ )
      {
      retval = false;
      }
    }
  return retval;
  }

static bool
iscasematch(const char *a1, const char *a2, const char *b1, const char *b2)
  {
  bool retval = true;
  while( a1 < a2 && b1 < b2 )
    {
    if( std::tolower((unsigned char)*a1++) != std::tolower((unsigned char)*b1++) )
      {
      retval = false;
      }
    }
  return retval;
  }

static
const char *
strstr( const char *haystack,
        const char *haystack_e,
        const char *needle,
        const char *needle_e)
  {
  const char *retval = NULL;
  const char *pend = haystack_e - (needle_e - needle);
  while( haystack <= pend )
    {
    if(ismatch(haystack, haystack_e, needle, needle_e))
      {
      retval = haystack;
      break;
      }
    haystack += 1;
    }
  return retval;
  }

static
const char *
strcasestr( const char *haystack,
            const char *haystack_e,
            const char *needle,
            const char *needle_e)
  {
  const char *retval = NULL;
  const char *pend = haystack_e - (needle_e - needle);
  while( haystack <= pend )
    {
    if(iscasematch(haystack, haystack_e, needle, needle_e))
      {
      retval = haystack;
      break;
      }
    haystack += 1;
    }
  return retval;
  }

static 
const char *
strlaststr( const char *haystack,
            const char *haystack_e,
            const char *needle,
            const char *needle_e)
  {
  const char *retval = NULL;
  const char *pend = haystack_e - (needle_e - needle);
  while( haystack <= pend )
    {
    if(ismatch(haystack, haystack_e, needle, needle_e))
      {
      retval = haystack;
      }
    haystack += 1;
    }
  return retval;
  }

static 
const char *
strcaselaststr( const char *haystack,
                const char *haystack_e,
                const char *needle,
                const char *needle_e)
  {
  const char *retval = NULL;
  const char *pend = haystack_e - (needle_e - needle);
  while( haystack <= pend )
    {
    if(iscasematch(haystack, haystack_e, needle, needle_e))
      {
      retval = haystack;
      }
    haystack += 1;
    }
  return retval;
  }


extern "C"
void 
__gg__substitute( cblc_field_t *dest,
            const cblc_field_t *arg1_f,
                  size_t        arg1_o,
                  size_t        arg1_s,
                  size_t        N,
            const uint8_t      *control)
  {
  // arg2 is the Group 1 triplet.
  // arg3 is the Group 2 triplet
  cblc_field_t **arg2_f = __gg__treeplet_1f;
  size_t        *arg2_o = __gg__treeplet_1o;
  size_t        *arg2_s = __gg__treeplet_1s;
  cblc_field_t **arg3_f = __gg__treeplet_2f;
  const size_t  *arg3_o = __gg__treeplet_2o;
  const size_t  *arg3_s = __gg__treeplet_2s;

  ssize_t retval_size;
  retval_size = 256;
  char  *retval = static_cast<char *>(malloc(retval_size));
  massert(retval);
  *retval = '\0';

  const char *haystack   = PTRCAST(char, (arg1_f->data + arg1_o));
  const char *haystack_e = haystack + arg1_s;

  ssize_t outdex = 0;

  const char **pflasts = static_cast<const char **>(malloc(N * sizeof(char *)));
  massert(pflasts);

  if( arg1_s == 0 )
    {
    exception_raise(ec_argument_function_e);
    goto bugout;
    }

  for( size_t i=0; i<N; i++ )
    {
    if( arg2_s[i] == 0 )
      {
      exception_raise(ec_argument_function_e);
      goto bugout;
      }
    if( control[i] & substitute_anycase_e )
      {
      if( control[i] & substitute_first_e )
        {
        pflasts[i] = strcasestr(haystack,
                                haystack_e,
                                PTRCAST(char, (arg2_f[i]->data+arg2_o[i])),
                                PTRCAST(char, (arg2_f[i]->data+arg2_o[i])) + arg2_s[i]);
        }
      else if( control[i] & substitute_last_e)
        {
        pflasts[i] = strcaselaststr(haystack,
                                haystack_e,
                                PTRCAST(char, (arg2_f[i]->data+arg2_o[i])),
                                PTRCAST(char, (arg2_f[i]->data+arg2_o[i])) + arg2_s[i]);
        }
      else
        {
        pflasts[i] = NULL;
        }
      }
    else
      {
      if( control[i] & substitute_first_e )
        {
        pflasts[i] = strstr(haystack,
                            haystack_e,
                            PTRCAST(char, (arg2_f[i]->data+arg2_o[i])),
                            PTRCAST(char, (arg2_f[i]->data+arg2_o[i])) + arg2_s[i]);
        }
      else if( control[i] & substitute_last_e)
        {
        pflasts[i] = strlaststr(haystack,
                                haystack_e,
                                PTRCAST(char, (arg2_f[i]->data+arg2_o[i])),
                                PTRCAST(char, (arg2_f[i]->data+arg2_o[i])) + arg2_s[i]);
        }
      else
        {
        pflasts[i] = NULL;
        }
      }
    }

  while( haystack < haystack_e )
    {
    bool did_something = false;
    for( size_t i=0; i<N; i++ )
      {
      // Let's make sure that there is enough room in the case that we add this
      // arg
      while( outdex - (ssize_t)arg2_s[i] + (ssize_t)arg3_s[i]
                                                                 > retval_size )
        {
        retval_size *= 2;
        retval = static_cast<char *>(realloc(retval, retval_size));
        massert(retval);
        }

      // We checked earlier for FIRST/LAST matches
      bool matched = pflasts[i] == haystack;
      if( !matched )
        {
        // It didn't match.  But if it was flagged as FIRST or LAST, we need
        // to skip it

        if( control[i] & (substitute_first_e|substitute_last_e) )
          {
          continue;
          }

        const char *needle   = PTRCAST(char, arg2_f[i]->data+arg2_o[i]);
        const char *needle_e = PTRCAST(char, arg2_f[i]->data+arg2_o[i]) + arg2_s[i];
        matched = (control[i] & substitute_anycase_e) && iscasematch(
                                                                 haystack,
                                                                 haystack_e,
                                                                 needle,
                                                                 needle_e);
        if( !matched )
          {
          matched = !(control[i] & substitute_anycase_e) && ismatch(haystack,
                                                                    haystack_e,
                                                                    needle,
                                                                    needle_e) ;
          }
        }
      if( matched )
        {
        haystack += arg2_s[i];
        memcpy(retval + outdex, arg3_f[i]->data + arg3_o[i], arg3_s[i]);
        outdex += arg3_s[i];
        did_something = true;
        break;
        }
      }
    if( !did_something )
      {
      while( outdex + 1 > retval_size )
        {
        retval_size *= 2;
        retval = static_cast<char *>(realloc(retval, retval_size));
        massert(retval);
        }
      retval[outdex++] = *haystack++;
      }
    }

  bugout:
  __gg__adjust_dest_size(dest, outdex);
  memcpy(dest->data, retval, outdex);

  free(pflasts);
  free(retval);
  }

extern "C"
void
__gg__locale_compare( cblc_field_t *dest,
                const cblc_field_t *arg1,
                      size_t        arg1_o,
                      size_t        arg1_s,
                const cblc_field_t *arg2,
                      size_t        arg2_o,
                      size_t        arg2_s,
                const cblc_field_t *arg_locale,
                      size_t        /*arg_locale_o*/,
                      size_t        /*arg_locale_s*/
                      )
  {
  char achretval[2] = "?";

  if( arg_locale )
    {
    // We don't yet know what to do with a locale
    exception_raise(ec_locale_missing_e);
    }
  else
    {
    // Default locale
    achretval[0] = '=';
    size_t length = std::min(arg1_s, arg2_s);
    for(size_t i=0; i<length; i++ )
      {
      if( (arg1->data+arg1_o)[i] < (arg2->data+arg2_o)[i] )
        {
        achretval[0] = '<';
        break;
        }
      if( (arg1->data+arg1_o)[i] > (arg2->data+arg2_o)[i] )
        {
        achretval[0] = '>';
        break;
        }
      }
    if( achretval[0] == '=' )
      {
      if( arg1_s < arg2_s )
        {
        achretval[0] = '<';
        }
      else if( arg1_s > arg2_s )
        {
        achretval[0] = '>';
        }
      }
    }

  __gg__adjust_dest_size(dest, 1);
  ascii_to_internal_str(achretval, 1);
  dest->data[0] = *achretval;
  }

extern "C"
void
__gg__locale_date(cblc_field_t *dest,
            const cblc_field_t *arg1,
                  size_t        arg1_o,
                  size_t        /*arg1_s*/,
            const cblc_field_t *arg_locale,
                  size_t        /*arg_locale_o*/,
                  size_t        /*arg_locale_s*/)
  {
  char ach[256] = "  ";

  if( arg_locale  )
    {
    // We don't yet know what to do with a locale
    exception_raise(ec_locale_missing_e);
    }
  else
    {
    // Default locale
    tm tm;
    memcpy(ach, arg1->data+arg1_o, 8);
    ach[8] = '\0';
    long ymd    = atoi(ach);
    tm.tm_year  = ymd/10000 - 1900;
    tm.tm_mon   = ymd/100 % 100;
    tm.tm_mday  = ymd % 100;
    strcpy(ach, nl_langinfo(D_FMT));
    strftime(ach, sizeof(ach), nl_langinfo(D_FMT), &tm);
    }

  __gg__adjust_dest_size(dest, strlen(ach));
  ascii_to_internal_str(ach, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  }

extern "C"
void
__gg__locale_time(cblc_field_t *dest,
            const cblc_field_t *arg1,
                  size_t        arg1_o,
                  size_t        /*arg1_s*/,
            const cblc_field_t *arg_locale,
                  size_t        /*arg_locale_o*/,
                  size_t        /*arg_locale_s*/)

  {
  char ach[256] = "  ";

  if( arg_locale)
    {
    // We don't yet know what to do with a locale
    exception_raise(ec_locale_missing_e);
    }
  else
    {
    // Default locale
    tm tm = {};
    memcpy(ach, arg1->data+arg1_o, 8);
    ach[8] = '\0';
    long hms    = atoi(ach);
    tm.tm_hour  = hms/10000;
    tm.tm_min   = hms/100 % 100;
    tm.tm_sec   = hms % 100;
    strftime(ach, sizeof(ach), nl_langinfo(T_FMT), &tm);
    }

  __gg__adjust_dest_size(dest, strlen(ach));
  ascii_to_internal_str(ach, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  }

extern "C"
void
__gg__locale_time_from_seconds( cblc_field_t *dest,
                          const cblc_field_t *arg1,
                                size_t        arg1_o,
                                size_t        arg1_s,
                          const cblc_field_t *arg_locale,
                                size_t        /*arg_locale_o*/,
                                size_t        /*arg_locale_s*/)
  {
  char ach[256] = "  ";

  if( arg_locale )
    {
    // We don't yet know what to do with a locale
    exception_raise(ec_locale_missing_e);
    }
  else
    {
    // Default locale
    tm tm = {};

    int rdigits=0;
    long seconds = (long)__gg__binary_value_from_qualified_field(&rdigits,
                                                                 arg1,
                                                                 arg1_o,
                                                                 arg1_s);
    tm.tm_hour   = seconds/3600;
    tm.tm_min    = ((seconds%3600) / 60) % 100;
    tm.tm_sec    = seconds % 100;
    strftime(ach, sizeof(ach), nl_langinfo(T_FMT), &tm);
    }

  __gg__adjust_dest_size(dest, strlen(ach));
  ascii_to_internal_str(ach, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  }
