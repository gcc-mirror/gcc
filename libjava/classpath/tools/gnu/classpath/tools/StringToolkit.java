/* gnu.classpath.tools.StringToolkit
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

/**
 *  Provides various String-related helper methods.
 *
 *  @author Julian Scheid
 */
public class StringToolkit
{
   /**
    *  Prevents instantiation.
    */
   private StringToolkit() {}

   /**
    *  Return <code>haystack</code> with all occurrences of
    *  <code>needle</code> replaced by </code>replacement</code>.
    *
    *  @param haystack the string to replace occurrences of <code>needle</code> in
    *  @param needle the substring to replace
    *  @param replacement the substring to replace <code>needle</code> with
    *
    *  @return <code>haystack</code> with all occurrences of
    *  <code>needle</code> replaced by </code>replacement</code>.
    */
   public static String replace(String haystack, String needle, String replacement)
   {
      int ndx = haystack.indexOf(needle);
      if (ndx < 0) {
         return haystack;
      }
      else {
         StringBuffer result = new StringBuffer();
         result.append(haystack.substring(0, ndx));
         result.append(replacement);
         ndx += needle.length();
         int ndx2;
         while ((ndx2 = haystack.indexOf(needle, ndx)) >= 0) {
            result.append(haystack.substring(ndx, ndx2));
            result.append(replacement);
            ndx = ndx2 + needle.length();
         }
         result.append(haystack.substring(ndx));
         return result.toString();
      }
   }
}
