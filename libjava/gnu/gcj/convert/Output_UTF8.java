/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

/**
 * Convert Unicode to UTF8.
 * @author Per Bothner <bothner@cygnus.com>
 * @date Match 1999.
 */

public class Output_UTF8 extends UnicodeToBytes
{
  public String getName() { return "UTF8"; }

  /** True if a surrogate pair should be emitted as a single UTF8 sequence.
   * Otherwise, a surrogate pair is treated as two separate characters.
   * Also, '\0' is emitted as {0} if true, and as {0xC0,0x80} if false. */
  public boolean standardUTF8;

  // Saves the previous char if it was a high-surrogate.
  char hi_part;
  // Value of incomplete character.
  int value;
  // Number of continuation bytes still to emit.
  int bytes_todo;

  public int write (char[] inbuffer, int inpos, int inlength)
  {
    int start_pos = inpos;
    int avail = buf.length - count;
    for (;;)
      {
	if (avail == 0 || (inlength == 0 && bytes_todo == 0))
	  break;
	// The algorithm is made more complicated because we want to write
	// at least one byte in the output buffer, if there is room for
	// that byte, and at least one input character is available.
	// This makes the code more robust, since client code will
	// always "make progress", even in the complicated cases,
	// where the output buffer only has room for only *part* of a
	// multi-byte sequence, or the input char buffer only has half
	// of a surrogate pair (when standardUTF8 is set), or both.

	// Handle continuation characters we did not have room for before.
	if (bytes_todo > 0)
	  {
	    do
	      {
		bytes_todo--;
		buf[count++] = (byte)
		  (((value >> (bytes_todo * 6)) & 0x3F) | 0x80);
		avail--;
	      }
	    while (bytes_todo > 0 && avail > 0);
	    continue;
	  }
	char ch = inbuffer[inpos++];
	inlength--;
	if (ch < 128 && (ch != 0 || standardUTF8))
	  {
	    avail--;
	    buf[count++] = (byte) ch;
	  }
	else if (ch <= 0x07FF)
	  {
	    buf[count++] = (byte) (0xC0 | (ch >> 6));
	    avail--;
	    value = ch;
	    bytes_todo = 1;
	  }
	else if (ch >= 0xD800 && ch <= 0xDFFF && standardUTF8)
	  {
	    if (ch <= 0xDBFF)  // High surrogates
	      {
		// The first byte is (0xF0 | value>>18), where value is the
		// Unicode scalar value of the combine character - which
		// we may not know yet.  But from substituting:
		// value == (hi-0xD800)*0x400+(lo-0xDC00)+0x10000,
		// hi==ch, and cancelling we get:
		buf[count++] = (byte) (0xF0 | ((ch-0xD800) >> 8));
		avail--;
		hi_part = ch;
	      }
	    else // Low surrogates
	      {
		value = (hi_part - 0xD800) * 0x400 + (ch - 0xDC00) + 0x10000;
		bytes_todo = 3;
	      }
	  }
	else
	  {
	    buf[count++] = (byte) (0xE0 | (ch >> 12));
	    value = ch;
	    avail--;
	    bytes_todo = 2;
	  }
      }
    return inpos - start_pos;
  }
}
