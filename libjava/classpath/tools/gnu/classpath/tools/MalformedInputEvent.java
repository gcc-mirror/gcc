/* gnu.classpath.tools.MalformedInputEvent
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA. */

package gnu.classpath.tools;

import java.util.EventObject;

/**
 *  Encapsulates information about malformed input encountered by a
 *  {@link NotifyingInputStreamReader}.
 *
 *  You can use {@link getSource()} to fetch a reference to the
 *  <code>NotifyingInputStreamReader</code> which encountered the
 *  malformed input.
 *
 *  @author Julian Scheid
 */
public class MalformedInputEvent
   extends EventObject
{
   private int lineNumber;
   private int columnNumber;
   private int length;

   MalformedInputEvent(NotifyingInputStreamReader source,
                       int lineNumber,
                       int columnNumber,
                       int length)
   {
      super(source);
      this.columnNumber = columnNumber;
      this.lineNumber = lineNumber;
      this.length = length;
   }

   /**
    *  Return the 1-based line number where the malformed input begins
    *  in the stream read by the
    *  <code>NotifyingInputStreamReader</code>.
    */
   public int getLineNumber()
   {
      return lineNumber;
   }

   /**
    *  Return the 0-based column number where the malformed input
    *  begins in the stream read by the
    *  <code>NotifyingInputStreamReader</code>.
    */
   public int getColumnNumber()
   {
      return columnNumber;
   }

   /**
    *  Return the length (in bytes) of the malformed input encountered
    *  by the <code>NotifyingInputStreamReader</code>. Note that a
    *  consecutive run of malformed input isn't necessarily reported
    *  as a whole; depending on the <code>Charset</code> and
    *  implementation details of <code>CharsetDecoder</code>, the run
    *  could be reported in chunks down to individual bytes.
    */
   public int getLength()
   {
      return length;
   }

   public String toString()
   {
      return "MalformedInputEvent{line=" + lineNumber
         + ",column=" + columnNumber
         + ",length=" + length
         + "}";
   }
}
