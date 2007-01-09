/* textPreProcessor.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.javax.swing.text.html.parser.support;

import gnu.javax.swing.text.html.parser.support.low.Constants;

/**
 * Pre - processes text in text parts of the html document.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class textPreProcessor
{
  /**
   * Pre - process non-preformatted text. \t, \r and \n mutate into spaces, then
   * multiple spaces mutate into single one, all whitespace around tags is
   * consumed. The content of the passed buffer is destroyed.
   * 
   * @param a_text A text to pre-process.
   */
  public char[] preprocess(StringBuffer a_text)
  {
    if (a_text.length() == 0)
      return null;

    char[] text = toCharArray(a_text);

    int a = 0;
    int b = text.length - 1;

    // Remove leading/trailing whitespace, leaving at most one character
    int len = text.length;
    while (a + 1 < len && Constants.bWHITESPACE.get(text[a])
           && Constants.bWHITESPACE.get(text[a + 1]))
      a++;

    while (b > a && Constants.bWHITESPACE.get(text[b])
               && Constants.bWHITESPACE.get(text[b - 1]))
      b--;

    a_text.setLength(0);

    boolean spacesWere = false;
    boolean spaceNow;
    char c;

    chars: for (int i = a; i <= b; i++)
      {
        c = text[i];
        spaceNow = Constants.bWHITESPACE.get(c);
        if (spacesWere && spaceNow)
          continue chars;
        if (spaceNow)
          a_text.append(' ');
        else
          a_text.append(c);
        spacesWere = spaceNow;
      }

    if (a_text.length() == text.length)
      {
        a_text.getChars(0, a_text.length(), text, 0);
        return text;
      }
    else
      return toCharArray(a_text);
  }

  /**
   * Pre - process pre-formatted text.
   * Heading/closing spaces and tabs preserved.
   * ONE  bounding \r, \n or \r\n is removed.
   * \r or \r\n mutate into \n. Tabs are
   * preserved.
   * The content of the passed buffer is destroyed.
   * @param text
   * @return
   */
  public char[] preprocessPreformatted(StringBuffer a_text)
  {
    if (a_text.length() == 0)
      return null;

    char[] text = toCharArray(a_text);

    int a = 0;
    int n = text.length - 1;
    int b = n;

    if (text [ 0 ] == '\n')
      a++;
    else
      {
        if (text [ 0 ] == '\r')
          {
            a++;
            if (text.length > 1 && text [ 1 ] == '\n')
              a++;
          }
      }

    if (text [ n ] == '\r')
      b--;
    else
      {
        if (text [ n ] == '\n')
          {
            b--;
            if (n > 0 && text [ n - 1 ] == '\r')
              b--;
          }
      }

    a_text.setLength(0);

    if (a > b)
      return null;

    char c;

    for (int i = a; i <= b; i++)
      {
        c = text [ i ];
        if (c == '\r')
          {
            if (i == b || text [ i + 1 ] != '\n')
              a_text.append('\n');
          }
        else
          a_text.append(c);
      }

    if (a_text.length() == text.length)
      {
        a_text.getChars(0, a_text.length(), text, 0);
        return text;
      }
    else
      return toCharArray(a_text);
  }

  /**
   * Return array of chars, present in the given buffer.
   * @param a_text The buffer
   * @return
   */
  private static char[] toCharArray(StringBuffer a_text)
  {
    char[] text = new char[ a_text.length() ];
    a_text.getChars(0, text.length, text, 0);
    return text;
  }
}
