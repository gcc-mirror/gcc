/* parameterDefaulter_Test.java --
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

import gnu.javax.swing.text.html.parser.HTML_401F;
import gnu.javax.swing.text.html.parser.htmlAttributeSet;
import gnu.javax.swing.text.html.parser.support.parameterDefaulter;

import javax.swing.text.AttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTML.Attribute;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class parameterDefaulter_Test
  extends TestCase
{
  parameterDefaulter defaulter;

  public void testDefaultValues()
  {
    AttributeSet d;
    d = defaulter.getDefaultParameters("FrAmE");
    assertEquals(d.getAttribute("scrolling"), "auto");
    d = defaulter.getDefaultParameters("input");
    assertEquals(d.getAttribute("type"), "text");

    htmlAttributeSet hma = new htmlAttributeSet();
    hma.setResolveParent(d);
    hma.addAttribute("ku", "1");
    hma.addAttribute(Attribute.ACTION, "sleep");

    assertEquals(hma.getAttribute("action"), "sleep");
    assertEquals(hma.getAttribute(Attribute.ACTION), "sleep");
    assertEquals(hma.getAttribute("ku"), "1");

    // Calling the parent:
    assertEquals(hma.getAttribute(Attribute.TYPE), "text");

    d = defaulter.getDefaultParameters("audrius");
    assertEquals(d.getAttribute("scrolling"), null);
  }

  protected void setUp()
  {
    defaulter = new parameterDefaulter(HTML_401F.getInstance());
  }

  protected void tearDown()
                   throws java.lang.Exception
  {
    defaulter = null;
    super.tearDown();
  }
}
