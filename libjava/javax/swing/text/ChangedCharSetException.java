/* ChangedCharSetException.java --
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package javax.swing.text;

import java.io.IOException;
import java.io.Serializable;

/**
 * The exception is thrown when the document charset is changed.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ChangedCharSetException
  extends IOException
  implements Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   * This value corresponds the version 1.4.
   */
  private static final long serialVersionUID = 9119851554465432389L;

  /**
   * The char set specification.
   */
  private final String m_charSetSpec;

  /**
   * The char set key.
   */
  private final boolean m_charSetKey;

  /**
   * Constructs a new char set exception with two additional parameters,
   * defining the circumstances under that the exception was raised.
   */
  public ChangedCharSetException(String charSetSpec, boolean charSetKey)
  {
    m_charSetSpec = charSetSpec;
    m_charSetKey = charSetKey;
  }

  /**
   * Get the value of the first parameter, previously passed to the
   * constructor.
   *
   * @return the value of the first parameter
   */
  public String getCharSetSpec()
  {
    return m_charSetSpec;
  }

  /**
   * Get the value of the second parameter, previously passed to the
   * constructor.
   *
   * @return the value of the second parameter
   */
  public boolean keyEqualsCharSet()
  {
    return m_charSetKey;
  }
}
