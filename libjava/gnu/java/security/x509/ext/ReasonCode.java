/* ReasonCode.java -- a reason code for a certificate revocation.
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

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

public class ReasonCode extends Extension.Value
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  public static final OID ID = new OID("2.5.29.21");

  public final int reason;

  // Constructor.
  // -------------------------------------------------------------------------

  public ReasonCode(final byte[] encoded) throws IOException
  {
    super(encoded);
    DERValue val = DERReader.read(encoded);
    if (val.getTag() != DER.ENUMERATED)
      throw new IOException("malformed CRLReason");
    reason = ((BigInteger) val.getValue()).intValue();
    if (reason < 0 || reason == 7 || reason > 10)
      throw new IOException("illegal reason: " + reason);
  }

  // Instance method.
  // -------------------------------------------------------------------------

  public int getReasonCode()
  {
    return reason;
  }

  public String toString()
  {
    return ReasonCode.class.getName() + " [ " + reason + " ]";
  }
}
