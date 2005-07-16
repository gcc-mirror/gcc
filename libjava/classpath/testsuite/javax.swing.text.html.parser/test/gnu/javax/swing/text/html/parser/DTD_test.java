/* DTD_test.java --
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

import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.Element;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class DTD_test
  extends TestCase
{
  static class D
    extends DTD
  {
    public D()
    {
      super("audrius");
    }

    public Element createElement(String n)
    {
      return getElement(n);
    }
  }

  public void testGetElement()
  {
    D d = new D();
    HTML.Tag[] tags = HTML.getAllTags();

    Element prehead = d.createElement("head");

    for (int i = 0; i < tags.length; i++)
      {
        Element e = d.createElement(tags [ i ].toString());
        String name = tags [ i ].toString();
        assertNotNull("Element creation", e);
        assertTrue("Element name", e.getName().equalsIgnoreCase(name));
      }

    // Test upper/lowercase
    Element e = d.createElement("head");

    assertNotNull("Element creation", e);
    assertTrue("Element name", e.getName().equalsIgnoreCase("head"));
    assertEquals(HTML.Tag.HEAD, HTML.getTag(e.name));
    assertEquals("Field assignment", d.head, e);

    assertEquals(prehead, e);
  }

  protected void setUp()
                throws Exception
  {
    super.setUp();
  }

  protected void tearDown()
                   throws Exception
  {
    super.tearDown();
  }
}
