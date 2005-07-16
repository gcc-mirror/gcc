/* Constants_Test.java --
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

import gnu.javax.swing.text.html.parser.support.low.Buffer;
import gnu.javax.swing.text.html.parser.support.low.Constants;
import gnu.javax.swing.text.html.parser.support.low.Token;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Constants_Test
  extends TestCase
{
  Constants c = new Constants();

  public void testCases()
  {
    verify("x stYle ", c.STYLE, "stYle");
    verify("x !style!", c.STYLE, "style");
    verify("x !Script!", c.SCRIPT, "Script");
    verify(" \r\t\n    z", c.WS, " \r\t\n    ");
    verify("123 ", c.NUMTOKEN, "123");
    verify("AaB123#", c.NUMTOKEN, "AaB123");
    verify("x-- ", c.DOUBLE_DASH, "--");
    verify("x--- ", c.DOUBLE_DASH, "--");

    verify("z&entitu ", c.ENTITY, "&entitu");

    verifyNull("x stYle");
    verifyNull("x !style");
    verifyNull("x !Script");
    verifyNull(" \r\t\n    ");
    verifyNull("123");
    verifyNull("AaB123");
    verifyNull("x--");
  }

  public void verify(String sequence, int kind, String image)
  {
    Token t = c.endMatches(new Buffer(sequence));
    assertEquals(kind, t.kind);
    assertEquals(image, t.getImage());
  }

  public void verifyNull(String sequence)
  {
    Token t = c.endMatches(new Buffer(sequence));
    assertNull("The end should not match any token", t);
  }
}
