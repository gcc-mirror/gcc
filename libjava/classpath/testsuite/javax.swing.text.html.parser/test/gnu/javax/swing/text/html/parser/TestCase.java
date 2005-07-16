/* TestCase.java --
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


/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class TestCase
{
  public TestCase()
  {
    try
      {
        setUp();
      }
    catch (Exception ex)
      {
        throw new RuntimeException(ex);
      }
  }

  public void assertEquals(String msg, Object a, Object b)
  {
    if (a == b)
      return;
    if (!a.equals(b))
      throw new RuntimeException(msg);
  }

  public void assertEquals(Object a, Object b)
  {
    if (a == b)
      return;
    if (!a.equals(b))
      throw new RuntimeException("Objects must be equal");
  }

  public void assertEquals(int a, int b)
  {
    if (a != b)
      throw new RuntimeException(a + "!=" + b);
  }

  public void assertEquals(String msg, int a, int b)
  {
    if (a != b)
      throw new RuntimeException(msg + ":" + a + "!=" + b);
  }

  public void assertEquals(boolean a, boolean b)
  {
    if (a != b)
      throw new RuntimeException(a + "!=" + b);
  }

  public void assertFalse(String msg, boolean a)
  {
    if (a)
      throw new RuntimeException(msg);
  }

  public void assertFalse(boolean a)
  {
    if (a)
      throw new RuntimeException("Must be false");
  }

  public void assertNotNull(String msg, Object a)
  {
    if (a == null)
      throw new RuntimeException(msg);
  }

  public void assertNull(String msg, Object a)
  {
    if (a != null)
      throw new RuntimeException(msg);
  }

  public void assertTrue(String msg, boolean a)
  {
    if (!a)
      throw new RuntimeException(msg);
  }

  public void assertTrue(boolean a)
  {
    if (!a)
      throw new RuntimeException("Must be true");
  }

  protected void setUp()
                throws Exception
  {
  }

  protected void tearDown()
                   throws Exception
  {
  }
}
