/* GeneralNames.java -- the GeneralNames object.
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

import java.net.InetAddress;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import gnu.java.security.OID;
import gnu.java.security.x509.X500DistinguishedName;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

public class GeneralNames
{

  // Instance methods.
  // -------------------------------------------------------------------------

  public static final int OTHER_NAME     = 0;
  public static final int RFC822_NAME    = 1;
  public static final int DNS_NAME       = 2;
  public static final int X400_ADDRESS   = 3;
  public static final int DIRECTORY_NAME = 4;
  public static final int EDI_PARTY_NAME = 5;
  public static final int URI            = 6;
  public static final int IP_ADDRESS     = 7;
  public static final int REGISTERED_ID  = 8;

  private List names;

  // Constructor.
  // -------------------------------------------------------------------------

  public GeneralNames(final byte[] encoded) throws IOException
  {
    names = new LinkedList();
    DERReader der = new DERReader(encoded);
    DERValue nameList = der.read();
    if (!nameList.isConstructed())
      throw new IOException("malformed GeneralNames");
    int len = 0;
    while (len < nameList.getLength())
      {
        DERValue name = der.read();
        List namePair = new ArrayList(2);
        if (name.getTagClass() != DER.APPLICATION)
          throw new IOException("malformed GeneralName");
        namePair.add(new Integer(name.getTag()));
        DERValue val = null;
        switch (name.getTag())
          {
          case RFC822_NAME:
          case DNS_NAME:
          case X400_ADDRESS:
          case URI:
            namePair.add(new String((byte[]) name.getValue()));
            break;

          case OTHER_NAME:
          case EDI_PARTY_NAME:
            namePair.add(name.getValue());
            break;

          case DIRECTORY_NAME:
            byte[] b = name.getEncoded();
            b[0] = (byte) (DER.CONSTRUCTED|DER.SEQUENCE);
            namePair.add(new X500DistinguishedName(b).toString());
            break;

          case IP_ADDRESS:
            namePair.add(InetAddress.getByAddress((byte[]) name.getValue())
                         .getHostAddress());
            break;

          case REGISTERED_ID:
            byte[] bb = name.getEncoded();
            bb[0] = (byte) DER.OBJECT_IDENTIFIER;
            namePair.add(new OID(bb).toString());
            break;

          default:
            throw new IOException("unknown tag " + name.getTag());
          }
        names.add(namePair);
        len += name.getEncodedLength();
      }
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public List getNames()
  {
    List l = new ArrayList(names.size());
    for (Iterator it = names.iterator(); it.hasNext(); )
      {
        List ll = (List) it.next();
        List pair = new ArrayList(2);
        pair.add(ll.get(0));
        if (ll.get(1) instanceof byte[])
          pair.add(((byte[]) ll.get(1)).clone());
        else
          pair.add(ll.get(1));
        l.add(Collections.unmodifiableList(pair));
      }
    return Collections.unmodifiableList(l);
  }

  public String toString()
  {
    return GeneralNames.class.getName() + " [ " + names + " ]";
  }
}
