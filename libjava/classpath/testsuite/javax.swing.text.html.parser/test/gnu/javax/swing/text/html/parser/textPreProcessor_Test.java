/* textPreProcessor_Test.java --
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


package test.gnu.javax.swing.text.html.parser;

import gnu.javax.swing.text.html.parser.support.textPreProcessor;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class textPreProcessor_Test
  extends TestCase
{
  textPreProcessor p = new textPreProcessor();

  public void testPreFormattedPreProcessing()
  {
    verifyF("rnrn...r.n.Q.Q.r.n.rn.Q...r.r.rn", "n...n.n.Q.Q.n.n.n.Q...n.n.");
    verifyF("...r.n.Q.Q.r.n.rn.Q...r.r.n", "...n.n.Q.Q.n.n.n.Q...n.n.");
    verifyF("r...r.n.Q.Q.r.n.rn.Q...r.r.n", "...n.n.Q.Q.n.n.n.Q...n.n.");
    verifyF("Q", "Q");
    verifyF(".", ".");
    verifyF("abc..\t..xyz", "abc..\t..xyz");
    verifyF("abcxyz", "abcxyz");
  }

  public void testStandardPreProcessing()
  {
    verifyS("...r.n.Q.Q.r.n.rn.Q...r.r.n", "Q.Q.Q");
    verifyS("r...r.n.Q.Q.r.n.rn.Q...r.r.n", "Q.Q.Q");
    verifyS("Q", "Q");
    verifyS(" ", null);
    verifyS(" \r\n", null);
    verifyS("abc..\t..xyz", "abc.xyz");
    verifyS("abcxyz", "abcxyz");
  }

  StringBuffer fromText(String x)
  {
    StringBuffer b = new StringBuffer();
    char c;
    for (int i = 0; i < x.length(); i++)
      {
        c = x.charAt(i);

        if (c == 'n')
          b.append('\n');
        else if (c == 'r')
          b.append('\r');
        else if (c == '.')
          b.append(' ');
        else
          b.append(c);
      }
    return b;
  }

  StringBuffer toText(String x)
  {
    StringBuffer b = new StringBuffer();
    char c;
    for (int i = 0; i < x.length(); i++)
      {
        c = x.charAt(i);

        if (c == '\n')
          b.append('n');
        else if (c == '\r')
          b.append('r');
        else if (c == ' ')
          b.append('.');
        else
          b.append(c);
      }
    return b;
  }

  void verifyF(String text, String result)
  {
    char[] pp = p.preprocessPreformatted(fromText(text));

    if (result == null && pp == null)
      return;

    String processed = new String(pp);

    processed = toText(processed).toString();

    if (!processed.equals(result))
      {
        System.err.println(result);
        System.out.println(processed);
      }
    assertEquals(text, result, processed);
  }

  void verifyS(String text, String result)
  {
    char[] pp = p.preprocess(fromText(text));

    if (result == null && pp == null)
      return;

    String processed = new String(pp);

    processed = toText(processed).toString();

    if (!processed.equals(result))
      {
        System.err.println(result);
        System.out.println(processed);
      }
    assertEquals(text, result, processed);
  }
}
