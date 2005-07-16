/* ReaderTokenizer_Test.java --
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


package test.gnu.javax.swing.text.html.parser.low;

import test.gnu.javax.swing.text.html.parser.TestCase;

import gnu.javax.swing.text.html.parser.support.low.Constants;
import gnu.javax.swing.text.html.parser.support.low.ReaderTokenizer;
import gnu.javax.swing.text.html.parser.support.low.Token;
import gnu.javax.swing.text.html.parser.support.low.node;
import gnu.javax.swing.text.html.parser.support.low.pattern;

import java.io.StringReader;

import java.util.ArrayList;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ReaderTokenizer_Test
  extends TestCase
{
  ReaderTokenizer rt = new ReaderTokenizer();

  public void testComplexToken()
                        throws Exception
  {
    String x = "< style  >x";

    pattern a =
      new pattern(new node[]
                  {
                    new node(Constants.BEGIN), new node(Constants.NUMTOKEN),
                    new node(Constants.END), new node(Constants.NUMTOKEN)
                  }
                 );

    pattern b =
      new pattern(new node[]
                  {
                    new node(Constants.BEGIN), new node(Constants.STYLE),
                    new node(Constants.END), new node(Constants.NUMTOKEN)
                  }
                 );

    pattern c =
      new pattern(new node[]
                  {
                    new node(Constants.BEGIN), new node(Constants.WS, true),
                    new node(Constants.STYLE), new node(Constants.WS, true),
                    new node(Constants.END), new node(Constants.NUMTOKEN)
                  }
                 );

    pattern d =
      new pattern(new node[]
                  {
                    new node(Constants.BEGIN), new node(Constants.WS, true),
                    new node(Constants.STYLE), new node(Constants.WS, true),
                    new node(Constants.END), new node(Constants.BEGIN)
                  }
                 );

    ReaderTokenizer rt = new ReaderTokenizer();
    rt.reset(new StringReader(x));

    assertFalse(a.matches(rt));
    assertFalse(b.matches(rt));
    assertTrue(c.matches(rt));
    assertFalse(d.matches(rt));
  }

  public void testReadingAndAhead()
                           throws Exception
  {
    ArrayList tokens = new ArrayList();
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < 10; i++)
      {
        String r = rs();
        b.append(" ");
        b.append(r + i);
        tokens.add(" ");
        tokens.add(r + i);
      }
    rt.reset(new StringReader(b.toString()));

    for (int i = 0; i < 10; i++)
      {
        for (int ah = 0; ah < 10; ah++)
          {
            Token ahead = rt.getTokenAhead(ah);
            if (i + ah >= tokens.size())
              {
                assertEquals(ahead.kind, rt.EOF);
              }
            else
              {
                if ((i + ah) % 2 == 0)
                  assertEquals(ahead.kind, rt.WS);
                else
                  {
                    assertEquals(ahead.getImage(), tokens.get(i + ah));
                    assertEquals(ahead.kind, rt.NUMTOKEN);
                  }
              }
          }

        Token r = rt.getNextToken();
        assertEquals(r.getImage(), tokens.get(i));
      }
  }

  private String rs()
  {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < 10 * Math.random(); i++)
      {
        b.append("l");
      }
    return b.toString();
  }
}
