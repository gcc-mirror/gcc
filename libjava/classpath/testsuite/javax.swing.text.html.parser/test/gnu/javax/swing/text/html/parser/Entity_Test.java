/* Entity_Test.java --
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

import javax.swing.text.html.parser.DTDConstants;
import javax.swing.text.html.parser.Element;
import javax.swing.text.html.parser.Entity;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Entity_Test
  extends TestCase
{
  private Element element = null;

  public void testName2type()
  {
    assertEquals("PUBLIC", Entity.name2type("PUBLIC"), DTDConstants.PUBLIC);
    assertEquals("SDATA", Entity.name2type("SDATA"), DTDConstants.SDATA);
    assertEquals("PI", Entity.name2type("PI"), DTDConstants.PI);
    assertEquals("STARTTAG", Entity.name2type("STARTTAG"), DTDConstants.STARTTAG);
    assertEquals("ENDTAG", Entity.name2type("ENDTAG"), DTDConstants.ENDTAG);
    assertEquals("MS", Entity.name2type("MS"), DTDConstants.MS);
    assertEquals("MD", Entity.name2type("MD"), DTDConstants.MD);
    assertEquals("SYSTEM", Entity.name2type("SYSTEM"), DTDConstants.SYSTEM);

    assertEquals("surely unknown ", Entity.name2type("audrius"),
                 DTDConstants.CDATA
                );
  }

  public void testPublicSystemGeneralParameter()
  {
    int[] pu_sy = new int[] { DTDConstants.PUBLIC, DTDConstants.SYSTEM, 0 };

    int[] gen_par =
      new int[] { DTDConstants.GENERAL, DTDConstants.PARAMETER, 0 };

    for (int ps = 0; ps < pu_sy.length; ps++)
      {
        for (int gp = 0; gp < gen_par.length; gp++)
          {
            Entity e = new Entity(null, 0, null);
            e.type = pu_sy [ ps ] | gen_par [ gp ];

            assertEquals(e.isGeneral(), gen_par [ gp ] == DTDConstants.GENERAL);
            assertEquals(e.isParameter(),
                         gen_par [ gp ] == DTDConstants.PARAMETER
                        );

            assertEquals((e.type & DTDConstants.SYSTEM) != 0,
                         pu_sy [ ps ] == DTDConstants.SYSTEM
                        );

            assertEquals((e.type & DTDConstants.PUBLIC) != 0,
                         pu_sy [ ps ] == DTDConstants.PUBLIC
                        );

            assertEquals((e.type & DTDConstants.GENERAL) != 0,
                         gen_par [ gp ] == DTDConstants.GENERAL
                        );

            assertEquals((e.type & DTDConstants.PARAMETER) != 0,
                         gen_par [ gp ] == DTDConstants.PARAMETER
                        );
          }
      }
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
