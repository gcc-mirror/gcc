/* BasicConstraints.java -- the basic constraints extension.
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package gnu.java.security.x509.ext;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

public class BasicConstraints extends Extension.Value
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  public static final OID ID = new OID("2.5.29.19");

  private final boolean ca;
  private final int pathLenConstraint;

  // Constructor.
  // -------------------------------------------------------------------------

  public BasicConstraints(final byte[] encoded) throws IOException
  {
    super(encoded);
    DERReader der = new DERReader(encoded);
    DERValue bc = der.read();
    if (!bc.isConstructed())
      throw new IOException("malformed BasicConstraints");
    DERValue val = bc;
    if (bc.getLength() > 0)
      val = der.read();
    if (val.getTag() == DER.BOOLEAN)
      {
        ca = ((Boolean) val.getValue()).booleanValue();
        if (val.getEncodedLength() < bc.getLength())
          val = der.read();
      }
    else
      ca = false;
    if (val.getTag() == DER.INTEGER)
      {
        pathLenConstraint = ((BigInteger) val.getValue()).intValue();
      }
    else
      pathLenConstraint = -1;
  }

  public BasicConstraints (final boolean ca, final int pathLenConstraint)
  {
    this.ca = ca;
    this.pathLenConstraint = pathLenConstraint;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public boolean isCA()
  {
    return ca;
  }

  public int getPathLengthConstraint()
  {
    return pathLenConstraint;
  }

  public byte[] getEncoded()
  {
    if (encoded == null)
      {
        List bc = new ArrayList (2);
        bc.add (new DERValue (DER.BOOLEAN, new Boolean (ca)));
        if (pathLenConstraint >= 0)
          bc.add (new DERValue (DER.INTEGER,
                                BigInteger.valueOf ((long) pathLenConstraint)));
        encoded = new DERValue (DER.CONSTRUCTED|DER.SEQUENCE, bc).getEncoded();
      }
    return (byte[]) encoded.clone();
  }

  public String toString()
  {
    return BasicConstraints.class.getName() + " [ isCA=" + ca +
      " pathLen=" + pathLenConstraint + " ]";
  }
}
