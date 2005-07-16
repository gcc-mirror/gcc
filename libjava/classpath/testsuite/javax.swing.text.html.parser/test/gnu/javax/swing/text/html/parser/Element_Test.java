/* Element_Test.java --
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

import javax.swing.text.html.parser.AttributeList;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.DTDConstants;
import javax.swing.text.html.parser.Element;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Element_Test
  extends TestCase
{
  private Element element = null;

  public void testAttributeGetter()
                           throws Exception
  {
    // Create a chain of 24 attributes:
    AttributeList list = new AttributeList("heading");
    AttributeList head = list;
    list.value = null;
    for (int i = 0; i < 24; i++)
      {
        AttributeList a = new AttributeList("a" + i);
        a.value = "v" + i;
        list.next = a;
        list = a;
      }

    Element e = DTD.getDTD("test").getElement("e");
    e.atts = head;

    for (int i = 0; i < 24; i++)
      {
        // Check if the name is found.
        assertEquals(e.getAttribute("a" + i).toString(), "a" + i);

        // Check if the attribute value is correct.
        assertEquals(e.getAttribute("a" + i).value, "v" + i);

        // Check if the attribute can be found by value.
        assertEquals(e.getAttributeByValue("v" + i).name, "a" + i);
      }

    // Check is the null value is searched correctly.
    assertEquals(e.getAttributeByValue(null).toString(), "heading");

    // Check for unknown attribute
    assertEquals(e.getAttribute("audrius"), null);

    // Check for unknown value
    assertEquals(e.getAttributeByValue("audrius"), null);
  }

  public void testName2type()
  {
    assertEquals(Element.name2type("CDATA"), DTDConstants.CDATA);
    assertEquals(Element.name2type("RCDATA"), DTDConstants.RCDATA);
    assertEquals(Element.name2type("EMPTY"), DTDConstants.EMPTY);
    assertEquals(Element.name2type("ANY"), DTDConstants.ANY);

    assertEquals(Element.name2type("audrius"), 0);
    assertEquals(Element.name2type("rcdata"), 0);
  }

  protected void setUp()
                throws Exception
  {
    super.setUp();
  }

  protected void tearDown()
                   throws Exception
  {
    element = null;
    super.tearDown();
  }
}
