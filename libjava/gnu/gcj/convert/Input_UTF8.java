/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

/**
 * Convert UTF8 to Unicode.
 * @author Per Bothner <bothner@cygnus.com>
 * @date March 1999.
 */

public class Input_UTF8 extends BytesToUnicode
{
  public String getName() { return "UTF8"; }

  int partial = 0;
  int partial_bytes_expected = 0;
  //int suggogate_second = -1;

  public int read (char[] outbuffer, int outpos, int count)
  {
    int origpos = outpos;
    for (;;)
      {
	if (outpos - origpos >= count)
	  break;
	if (inpos >= inlength)
	  break;
	int b = inbuffer[inpos++];
	if (b >= 0)
	  outbuffer[outpos++] = (char) b;
	else
	  {
	    if ((b & 0xC0) == 0x80) // Continuation byte
	      {
		partial = (partial << 6) | (b & 0x3F);
		--partial_bytes_expected;
		if (partial_bytes_expected == 1)
		  {
		    if (partial > (0xFFFF>>6))
		      {
			// The next continuation byte will cause the result
			// to exceed 0xFFFF, so we must use a surrogate pair.
			// The "Unicode scalar value" (see D28 in section 3.7
			// of the Unicode Standard 2.0) is defined as:
			// value == (hi-0xD800)*0x400+(lo-0xDC00)+0x10000,
			// where (hi, lo) is the Unicode surrogate pair.
			// After reading the first three bytes, we have:
			// partial == (value >> 6).
			// Substituting and simplifying, we get:
			// partial == (hi-0xD800)*0x10+((lo-0xDC00)>>6)+0x400.
			// The definition lo>=0xDC00 && lo<=0xDFFF implies
			// that (lo-0xDC00)>>6 is in the range 0..15.
			// Hence we can infer (partial-0x400)>>4 == (hi-0xDB00)
			// and we can emit the high-surrogate without waiting
			// for the final byte:
			outbuffer[outpos++] = (char) (0xDA00+(partial>>4));

			// Now we want to set it up so that when we read
			// the final byte on the next iteration, we will
			// get the low-surrogate without special handling.
			// I.e. we want:
			// lo == (next_partial << 6) | (next & 0x3F)
			// where next is the next input byte and next_partial
			// is the value of partial at the end of this
			// iteration.  This implies:  next_partial == lo >> 6.
			// We can simplify the previous:
			// partial == (hi-0xD800)*0x10+((lo-0xDC00)>>6)+0x400,
			// to: partial == (hi-0xD800)*0x10+(lo>>6)+0x90.
			// Inserting the values of hi and next_partial,
			// and simplifying, we get:  partial ==
			// ( (partial-0x400)&~0xF) + next_partial + 0x90.
			// Solving for next_partial, we get:
			// next_partial = partial+0x400-0x90-(partial&~0xF):
			// or: next_partial = (partial&0xF) + 0x370.  Hence:
			partial = (partial & 0xF) + 0x370;
		      }
		  }
		else if (partial_bytes_expected == 0)
		  {
		    outbuffer[outpos++] = (char) partial;
		    partial = 0;
		    partial_bytes_expected = 0;
		  }
	      }
	    else // prefix byte
	      {
		if ((b & 0xE0) == 0xC0)
		  {
		    partial = b & 0x1F;
		    partial_bytes_expected = 1;
		  }
		else if ((b & 0xF0) == 0xE0)
		  {
		    partial = b & 0xF;
		    partial_bytes_expected = 2;
		  }
		else
		  {
		    partial = b & 7;
		    partial_bytes_expected = 3;
		  }
	      }
	  }
      }
    return outpos - origpos;
  }
}
