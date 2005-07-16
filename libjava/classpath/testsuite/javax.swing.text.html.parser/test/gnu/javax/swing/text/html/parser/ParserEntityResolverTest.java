/* ParserEntityResolverTest.java --
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
import gnu.javax.swing.text.html.parser.support.Parser;

import java.lang.reflect.Method;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ParserEntityResolverTest
  extends TestCase
{
  /* Testing private methods of entity resolver. */
  public void testResolver()
                    throws Exception
  {
    Parser p =
      new Parser(HTML_401F.getInstance())
      {
        public void error(String a, String b)
        {
        }
      };

    Method rn =
      p.getClass().getSuperclass().getDeclaredMethod("resolveNamedEntity",
                                                     new Class[] { String.class }
                                                    );
    rn.setAccessible(true);

    assertEquals(exe(p, rn, "&amp"), "&");
    assertEquals(exe(p, rn, "&AMP"), "&");
    assertEquals(exe(p, rn, "&amp"), "&");
    assertEquals(exe(p, rn, "&amP"), "&");

    assertEquals(exe(p, rn, "&;"), "&;");
    assertEquals(exe(p, rn, "&audrius;"), "&audrius;");

    rn =
      p.getClass().getSuperclass().getDeclaredMethod("resolveNumericEntity",
                                                     new Class[] { String.class }
                                                    );
    rn.setAccessible(true);

    assertEquals(exe(p, rn, "&#x55"), "U");
    assertEquals(exe(p, rn, "&#X55"), "U");
    assertEquals(exe(p, rn, "&#61"), "=");
    assertEquals(exe(p, rn, "&#61"), "=");

    assertEquals(exe(p, rn, "&#audrius"), "?");
  }

  private String exe(Parser p, Method m, String arg)
              throws Exception
  {
    Object[] o = new Object[ 1 ];
    o [ 0 ] = arg;
    return m.invoke(p, o).toString();
  }
}
