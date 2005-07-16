/* Parser_Test.java --
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

import java.io.StringReader;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.TreeSet;

import javax.swing.text.AttributeSet;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.HTMLEditorKit.ParserCallback;
import javax.swing.text.html.parser.ParserDelegator;
import javax.swing.text.html.parser.TagElement;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Parser_Test
  extends HTMLEditorKit.ParserCallback
{
  public boolean hideImplied = true;
  protected StringBuffer out = new StringBuffer();
  AttributeSet atts = new SimpleAttributeSet();

  public void generate(String x, String comment)
                throws Exception
  {
    String prolog = "<html><head></head><body>";
    String epilog = "</body></html>";
    String html = x; // prolog+x+epilog;
    System.out.println("// Test " + comment + ".");
    System.out.println("v.verify(\"" + html + "\",\n  \"" + verify(html, null) +
                       "\");"
                      );
  }

  public void handleComment(char[] parm1, int position)
  {
    out.append("{" + new String(parm1) + "}");
  }

  public void handleEndTag(HTML.Tag tag, int position)
  {
    out.append("</" + tag + ">");
  }

  public void handleSimpleTag(HTML.Tag tag, MutableAttributeSet attributes,
                              int position
                             )
  {
    if (tag.toString().equals("#pcdata"))
      return;
    out.append("<" + tag);
    dumpAttributes(attributes);
    out.append("/>");
  }

  public void handleStartTag(HTML.Tag tag, MutableAttributeSet attributes,
                             int position
                            )
  {
    out.append("<" + tag);
    dumpAttributes(attributes);
    out.append('>');
  }

  public void handleText(char[] chars, int position)
  {
    out.append("'" + new String(chars) + "'");
  }

  public String verify(String html, String trace)
                throws Exception
  {
    out.setLength(0);

    HTMLEditorKit.ParserCallback callback = this;
    ParserDelegator delegator = new ParserDelegator();
    delegator.parse(new StringReader(html), callback, true);

    String ou = out.toString();
    if (trace != null)
      {
        if (!ou.equals(trace))
          {
            System.err.println("Unable to parse '" + html + "':");
            System.err.println("    expected: '" + trace + "',");
            System.out.println("    returned: '" + ou + "'.");
            throw new Exception("'" + html + "' -> '" + ou + "' expected '" +
                                trace + "'"
                               );
          }
      }
    return ou;
  }

  protected void dumpAttributes(AttributeSet atts)
  {
    Enumeration enum = atts.getAttributeNames();

    // Sort them to ensure the same order every time:
    TreeSet t = new TreeSet();
    while (enum.hasMoreElements())
      t.add(enum.nextElement().toString());

    Iterator iter = t.iterator();

    while (iter.hasNext())
      {
        String a = iter.next().toString();

        if (hideImplied)
          if (a.equals("_implied_"))
            continue;

        String v = atts.getAttribute(a).toString();
        out.append(" " + a + "='" + v + "'");
      }
  }
}
