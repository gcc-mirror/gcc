/* ParserTest.java --
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

import gnu.javax.swing.text.html.parser.support.Parser;

import java.io.PrintStream;

import java.util.Enumeration;

import javax.swing.text.AttributeSet;
import javax.swing.text.html.parser.Element;
import javax.swing.text.html.parser.TagElement;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ParserTest
  extends gnu.javax.swing.text.html.parser.support.Parser
{
  PrintStream out = System.out;
  StringBuffer errors = new StringBuffer();

  public ParserTest()
  {
    super(gnu.javax.swing.text.html.parser.HTML_401F.getInstance());
  }

  public static void main(String[] args)
  {
    String sx;
    sx =
      "<html><head></head><body><table>< tbody><tr ><   td >C_0_0< td>C_0_1<   td >C_0_2<   /td  >< td  >C_0_3<td>C_0_4<   /td></tr ></tbody></table></body></html>";
    try
      {
        System.out.println(sx);

        ParserTest t = new ParserTest();
        t.parse(new java.io.StringReader(sx));
        System.out.println("\nErrors:");
        System.out.println(t.errors);
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }

  protected void handleComment(char[] parm1)
  {
    out.print("{" + new String(parm1) + "}");
  }

  protected void handleEOFInComment()
  {
    out.print(" [EOF in comment] ");
  }

  protected void handleEmptyTag(TagElement tag)
                         throws javax.swing.text.ChangedCharSetException
  {
    out.print("<" + tag);

    javax.swing.text.AttributeSet atts = getAttributes();
    dumpAttributes(atts);
    out.print("/>");
  }

  protected void handleEndTag(TagElement tag)
  {
    out.print("</" + tag + "> ");
  }

  protected void handleError(int line, String message)
  {
    errors.append(message);
    errors.append('\n');
  }

  protected void handleStartTag(TagElement tag)
  {
    out.print("<" + tag);

    javax.swing.text.AttributeSet atts = getAttributes();
    dumpAttributes(atts);
    out.print('>');
  }

  protected void handleText(char[] parm1)
  {
    out.print("'" + new String(parm1) + "'");
  }

  protected void handleTitle(char[] parm1)
  {
    out.print(" [ Title: " + new String(parm1) + "] ");
  }

  protected void markFirstTime(Element element)
  {
    out.print("(1:" + element + ")");
  }

  private void dumpAttributes(AttributeSet atts)
  {
    Enumeration enum = atts.getAttributeNames();
    while (enum.hasMoreElements())
      {
        String a = enum.nextElement().toString();
        String v = (String) atts.getAttribute(a);
        out.print(" " + a + "='" + v + "'");
      }
  }
}
