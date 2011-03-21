/* GeneralSubtree.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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


package gnu.java.security.x509.ext;

import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.IOException;
import java.math.BigInteger;

/**
 * The GeneralSubtree structure, a part of the {@link NameConstraints}
 * extension.
 *
 * <pre>
  GeneralSubtree ::= SEQUENCE {
    base                    GeneralName,
    minimum         [0]     BaseDistance DEFAULT 0,
    maximum         [1]     BaseDistance OPTIONAL }

  BaseDistance ::= INTEGER (0..MAX)</pre>
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class GeneralSubtree
{
  private final GeneralName base;
  private final int minimum;
  private final int maximum;

  public GeneralSubtree(byte[] encoded) throws IOException
  {
    DERReader reader = new DERReader(encoded);
    DERValue generalSubtree = reader.read();

    if (!generalSubtree.isConstructed())
      throw new IOException("malformed GeneralSubtree");

    DERValue generalName = reader.read();
    base = new GeneralName(generalName.getEncoded());
    if (generalName.isConstructed())
      reader.skip(generalName.getLength());

    int len = generalName.getEncodedLength();
    if (len < generalSubtree.getLength())
      {
        DERValue distance = reader.read();
        if (distance.getTag() == 0)
          {
            minimum = ((BigInteger) distance.getValue()).intValue();
            len += distance.getEncodedLength();
            if (len < generalSubtree.getLength())
              {
                distance = reader.read();
                if (distance.getTag() != 1)
                  throw new IOException("unexpected tag "
                                        + distance.getTag() +
                                        " (expected 1 for GeneralSubtree maximum distance)");
                maximum = ((BigInteger) distance.getValue()).intValue();
              }
            else
              {
                maximum = -1;
              }
          }
        else if (distance.getTag() == 1)
          {
            minimum = 1;
            maximum = ((BigInteger) distance.getValue()).intValue();
          }
        else
          {
            throw new IOException("unexpected tag " + distance.getTag()
                                  + " (expected 0 or 1 for GeneralSubtree distance)");
          }
      }
    else
      {
        minimum = 0;
        maximum = -1;
      }
  }

  /**
   * Returns the base name.
   *
   * @return The base name.
   */
  public GeneralName base()
  {
    return base;
  }

  /**
   * Returns the minimum base distance, possibly zero.
   *
   * @return The minimum base distance.
   */
  public int minimum()
  {
    return minimum;
  }

  /**
   * Returns the maximum base distance, or -1 if this value was not specified.
   *
   * @return The maximum base distance.
   */
  public int maximum()
  {
    return maximum;
  }

  public String toString()
  {
    return (GeneralSubtree.class.getName() + " [ base=" + base
            + "; minimum=" + minimum + "; maximim=" + maximum
            + " ]");
  }
}
