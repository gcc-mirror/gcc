/* HTML_randomTable.java --
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

import java.util.Random;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class HTML_randomTable
  extends TestCase
{
  class table
  {
    final String[][] rows;
    final boolean caption = r.nextBoolean();

    table()
    {
      int nrows = r.nextInt(5) + 1;
      rows = new String[ nrows ][];
      for (int i = 0; i < rows.length; i++)
        {
          int ncol = r.nextInt(5) + 1;
          rows [ i ] = new String[ ncol ];
          for (int j = 0; j < rows [ i ].length; j++)
            {
              rows [ i ] [ j ] = "C_" + i + "_" + j;
            }
        }
    }

    public String getHtml()
    {
      StringBuffer b = new StringBuffer("<html><head></head><body><table>");
      if (caption)
        b.append("<caption>capt</caption>");
      if (r.nextBoolean())
        b.append("<" + s() + "tbody" + s() + ">");
      for (int row = 0; row < rows.length; row++)
        {
          b.append("<" + s() + "tr" + s() + ">");
          for (int col = 0; col < rows [ row ].length; col++)
            {
              b.append("<" + s() + "td" + s() + ">");
              b.append(rows [ row ] [ col ]);
              if (r.nextBoolean())
                b.append("<" + s() + "/" + "td" + s() + ">");
            }
          if (r.nextBoolean())
            b.append("<" + s() + "/" + "tr" + s() + ">");
        }
      b.append("</tbody></table></body></html>");
      return b.toString();
    }

    public String getTrace()
    {
      StringBuffer b = new StringBuffer("<html><head></head><body><table>");
      if (caption)
        b.append("<caption>'capt'</caption>");
      b.append("<tbody>");
      for (int row = 0; row < rows.length; row++)
        {
          b.append("<tr>");
          for (int col = 0; col < rows [ row ].length; col++)
            {
              b.append("<td>'" + rows [ row ] [ col ] + "'</td>");
            }
          b.append("</tr>");
        }
      b.append("</tbody></table></body></html>");
      return b.toString();
    }

    void test()
       throws Exception
    {
      String trace = getTrace();
      String html = getHtml();
      v.verify(html, trace);
    }
  }

  Parser_Test v = new Parser_Test();
  Random r = new Random();

  public HTML_randomTable()
                   throws Exception
  {
  }

  public String s()
  {
    if (r.nextBoolean())
      return "";

    StringBuffer b = new StringBuffer();
    int spc = r.nextInt(4);
    for (int i = 0; i < spc; i++)
      {
        b.append(' ');
      }
    return b.toString();
  }

  /**
   * Try 1001 variable randomly generated table.
   */
  public void testTableParsing()
                        throws Exception
  {
    v.hideImplied = true;
    for (int i = 0; i < 1001; i++)
      {
        new table().test();
      }
  }
}
