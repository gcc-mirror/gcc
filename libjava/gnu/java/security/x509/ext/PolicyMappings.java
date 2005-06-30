/* PolicyMappings.java -- policy mappings extension.
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


package gnu.java.security.x509.ext;

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class PolicyMappings extends Extension.Value
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  public static final OID ID = new OID("2.5.29.33");

  private final Map mappings;

  // Constructor.
  // -------------------------------------------------------------------------

  public PolicyMappings(final byte[] encoded) throws IOException
  {
    super(encoded);
    DERReader der = new DERReader(encoded);
    DERValue maps = der.read();
    if (!maps.isConstructed())
      throw new IOException("malformed PolicyMappings");
    int len = 0;
    HashMap _mappings = new HashMap();
    while (len < maps.getLength())
      {
        DERValue map = der.read();
        if (!map.isConstructed())
          throw new IOException("malformed PolicyMapping");
        DERValue val = der.read();
        if (val.getTag() != DER.OBJECT_IDENTIFIER)
          throw new IOException("malformed PolicyMapping");
        OID issuerPolicy = (OID) val.getValue();
        val = der.read();
        if (val.getTag() != DER.OBJECT_IDENTIFIER)
          throw new IOException("malformed PolicyMapping");
        OID subjectPolicy = (OID) val.getValue();
        _mappings.put(issuerPolicy, subjectPolicy);
        len += map.getEncodedLength();
      }
    mappings = Collections.unmodifiableMap(_mappings);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public OID getSubjectDomainPolicy(OID issuerDomainPolicy)
  {
    return (OID) mappings.get(issuerDomainPolicy);
  }

  public String toString()
  {
    return PolicyMappings.class.getName() + " [ " + mappings + " ]";
  }
}
