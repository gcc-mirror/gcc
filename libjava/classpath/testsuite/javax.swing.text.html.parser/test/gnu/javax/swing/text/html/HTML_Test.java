/* HTML_Test.java -- HTML parser test.
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


package test.gnu.javax.swing.text.html;

import test.gnu.javax.swing.text.html.parser.TestCase;

import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;

public class HTML_Test
  extends TestCase
{
  /**
   * By the language definition, HTML tags and attributes are case
   * insensitive. Hence if it is not clearly specified, in which case
   * the tag name must be, it should be expected to come as in
   * lowercase, as in uppercase. This should be true for HTML.getTag(String)
   * and for HTML.getAttributeKey(String).
   *
   * In some implementations these two functions may be case sensitive.
   * As this requirement is not mentioned in the documentation,
   * and also it is not documented, in which case the name must be supplied,
   * this will be reported as an error in this test.
   * The GNU CLASSPATH implementation is case insensitive.
   */
  public void testCaseSensitivity()
  {
    String def = "case sensitivity";
    assertEquals("html=Html", HTML.getTag("html"), HTML.getTag("HtmL"));
    assertEquals("html=HTML", HTML.getTag("html"), HTML.getTag("HTML"));
    assertEquals("size=SIZE", HTML.getAttributeKey("size"),
                 HTML.getAttributeKey("SIZE")
                );
    assertEquals("size=SizE", HTML.getAttributeKey("size"),
                 HTML.getAttributeKey("SizE")
                );
  }

  public void testConstructor()
  {
    new HTML();
  }

  public void testGetAttributeKey()
  {
    // Test the known tags.
    String[] mine = toStrings(HTML.getAllAttributeKeys());

    for (int i = 0; i < mine.length; i++)
      assertNotNull(mine [ i ], HTML.getAttributeKey(mine [ i ]));

    // Test the unknown tag.
    assertNull("surely unknown", HTML.getTag("audrius"));
  }

  public void testGetIntegerAttributeValue()
  {
    SimpleAttributeSet ase = new SimpleAttributeSet();
    ase.addAttribute(HTML.getAttributeKey("size"), "222");
    assertEquals(222,
                 HTML.getIntegerAttributeValue(ase,
                                               HTML.getAttributeKey("size"), 333
                                              )
                );

    assertEquals(333,
                 HTML.getIntegerAttributeValue(ase,
                                               HTML.getAttributeKey("href"), 333
                                              )
                );
  }

  public void testGetTag()
  {
    // known tags:
    String[] mine = toStrings(HTML.getAllTags());

    for (int i = 0; i < mine.length; i++)
      assertNotNull(mine [ i ], HTML.getTag(mine [ i ]));

    // unknown tag
    assertNull("surely unknown", HTML.getTag("audrius"));
  }

  private String[] toStrings(Object[] objs)
  {
    String[] a = new String[ objs.length ];

    for (int i = 0; i < a.length; i++)
      a [ i ] = objs [ i ].toString();

    return a;
  }
}
