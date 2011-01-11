/* GeneralName.java -- a GeneralName.
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

import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.x509.Util;

import java.io.IOException;
import java.util.Arrays;

/**
 * The GeneralName structure from X.509.
 *
 * <pre>
  GeneralName ::= CHOICE {
    otherName                       [0]     OtherName,
    rfc822Name                      [1]     IA5String,
    dNSName                         [2]     IA5String,
    x400Address                     [3]     ORAddress,
    directoryName                   [4]     Name,
    ediPartyName                    [5]     EDIPartyName,
    uniformResourceIdentifier       [6]     IA5String,
    iPAddress                       [7]     OCTET STRING,
    registeredID                    [8]     OBJECT IDENTIFIER }

  OtherName ::= SEQUENCE {
    type-id    OBJECT IDENTIFIER,
    value      [0] EXPLICIT ANY DEFINED BY type-id }

  EDIPartyName ::= SEQUENCE {
    nameAssigner            [0]     DirectoryString OPTIONAL,
    partyName               [1]     DirectoryString }
</pre>
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class GeneralName
{
  public static enum Kind
  {
    otherName (0),
    rfc822Name (1),
    dNSName (2),
    x400Address (3),
    directoryName (4),
    ediPartyName (5),
    uniformResourceIdentifier (6),
    iPAddress (7),
    registeredId (8);

    private int tag;

    private Kind(int tag)
    {
      this.tag = tag;
    }

    public static Kind forTag(final int tag)
    {
      switch (tag)
      {
        case 0: return otherName;
        case 1: return rfc822Name;
        case 2: return dNSName;
        case 3: return x400Address;
        case 4: return directoryName;
        case 5: return ediPartyName;
        case 6: return uniformResourceIdentifier;
        case 7: return iPAddress;
        case 8: return registeredId;
      }

      throw new IllegalArgumentException("invalid tag: " + tag);
    }

    public int tag()
    {
      return tag;
    }
  };

  private final Kind kind;
  private final byte[] name;
  private final byte[] encoded;

  public GeneralName(byte[] encoded) throws IOException
  {
    DERReader reader = new DERReader(encoded);
    DERValue value = reader.read();

    if (value.getTagClass() != DER.CONTEXT)
      throw new IOException("malformed GeneralName");

    this.encoded = value.getEncoded();

    kind = Kind.forTag(value.getTag());
    switch (kind)
    {
      case otherName:
        name = value.getEncoded();
        name[0] = (byte) (DER.CONSTRUCTED | DER.SEQUENCE);
        // Skip the two fields of the name.
        reader.read();  // OID
        reader.read();  // Octet string
        break;

      case rfc822Name:
        name = (byte[]) value.getValue();
        break;

      case dNSName:
        name = (byte[]) value.getValue();
        break;

      case x400Address:
        name = (byte[]) value.getValue();
        break;

      case directoryName:
        name = value.getEncoded();
        name[0] = (byte) (DER.CONSTRUCTED | DER.SEQUENCE);
        break;

      case ediPartyName:
        name = value.getEncoded();
        name[0] = (byte) (DER.CONSTRUCTED | DER.SEQUENCE);
        break;

      case uniformResourceIdentifier:
        name = (byte[]) value.getValue();
        break;

      case iPAddress:
        name = (byte[]) value.getValue();
        break;

      case registeredId:
        name = value.getEncoded();
        name[0] = DER.OBJECT_IDENTIFIER;
        break;

      default:
        name = null; // Not reached.
    }
  }

  public GeneralName(Kind kind, byte[] name)
  {
    this.kind = kind;
    this.name = (byte[]) name.clone();
    this.encoded = null;
  }

  public Kind kind()
  {
    return kind;
  }

  public byte[] name()
  {
    return (byte[]) name.clone();
  }

  public byte[] encoded()
  {
    try
      {
        return (byte[]) encoded.clone();
      }
    catch (NullPointerException npe)
      {
        return null;
      }
  }

  public boolean equals(Object o)
  {
    try
      {
        GeneralName that = (GeneralName) o;
        return (that.kind() == kind() && Arrays.equals(name, that.name));
      }
    catch (ClassCastException cce)
      {
        return false;
      }
  }

  public String toString()
  {
    return (super.toString() + " [ kind=" + kind + "; name=" +
            Util.hexDump(name, "") + " ]");
  }
}
