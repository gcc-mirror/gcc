/* TrustedAuthorities.java
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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import gnu.javax.net.ssl.provider.Extension.Value;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Iterator;
import java.util.NoSuchElementException;

import javax.security.auth.x500.X500Principal;

/**
 * The trusted authorities hello extension.
 *
 * <pre>
struct {
  TrustedAuthority trusted_authorities_list&lt;0..2^16-1&gt;;
} TrustedAuthorities;

struct {
  IdentifierType identifier_type;
  select (identifier_type) {
    case pre_agreed: struct {};
    case key_sha1_hash: SHA1Hash;
    case x509_name: DistinguishedName;
    case cert_sha1_hash: SHA1Hash;
  } identifier;
} TrustedAuthority;

enum {
  pre_agreed(0), key_sha1_hash(1), x509_name(2),
  cert_sha1_hash(3), (255)
} IdentifierType;

opaque DistinguishedName&lt;1..2^16-1&gt;;</pre>
 *
 * @author csm
 */
public class TrustedAuthorities extends Value
  implements Iterable<TrustedAuthorities.TrustedAuthority>
{
  private final ByteBuffer buffer;

  public TrustedAuthorities(final ByteBuffer buffer)
  {
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
  }

  // XXX really implement Builder.

  public int length()
  {
    return 2 + (buffer.getShort(0) & 0xFFFF);
  }

  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().limit(length());
  }

  public int size()
  {
    int len = buffer.getShort(0) & 0xFFFF;
    int n = 0;
    for (int i = 2; i < len; i++)
      {
        TrustedAuthority auth =
          new TrustedAuthority((ByteBuffer) buffer.duplicate().position(i));
        i += auth.length();
        n++;
      }
    return n;
  }

  public TrustedAuthority get(final int index)
  {
    int len = buffer.getShort(0) & 0xFFFF;
    int n = 0;
    int i = 2;
    while (i < len && n <= index)
      {
        TrustedAuthority auth =
          new TrustedAuthority((ByteBuffer) buffer.duplicate().position(i));
        if (n == index)
          return auth;
        i += auth.length();
        n++;
      }
    throw new IndexOutOfBoundsException();
  }

  public String toString()
  {
    return toString(null);
  }

  public String toString(String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    if (prefix != null) out.print(prefix);
    out.println("struct {");
    String subprefix = "  ";
    if (prefix != null)
      subprefix = prefix + subprefix;
    for(TrustedAuthority ta : this)
      out.println(ta);
    if (prefix != null) out.print(prefix);
    out.print("} TrustedAuthorities;");
    return str.toString();
  }

  public Iterator<TrustedAuthority> iterator()
  {
    return new AuthoritiesIterator();
  }

  public class AuthoritiesIterator implements Iterator<TrustedAuthority>
  {
    private int index;

    public AuthoritiesIterator()
    {
      index = 0;
    }

    public TrustedAuthority next() throws NoSuchElementException
    {
      try
        {
          return get(index++);
        }
      catch (IndexOutOfBoundsException ioobe)
        {
          throw new NoSuchElementException();
        }
    }

    public boolean hasNext()
    {
      return index < size();
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }
  }

  public static class TrustedAuthority implements Constructed
  {
    private final ByteBuffer buffer;

    public TrustedAuthority(final ByteBuffer buffer)
    {
      this.buffer = buffer;
    }

    public int length()
    {
      switch (type().getValue())
      {
        case 0: return 1;
        case 1:
        case 3: return 21;
        case 2: return 3 + (buffer.getShort(1) & 0xFFFF);
      }
      throw new IllegalArgumentException("unknown authority type");
    }

    public byte[] sha1Hash()
    {
      IdentifierType t = type();
      if (t != IdentifierType.CERT_SHA1_HASH
          && t != IdentifierType.KEY_SHA1_HASH)
        throw new IllegalArgumentException(t + " does not have a hash value");
      byte[] b = new byte[20];
      ((ByteBuffer) buffer.duplicate().position(1)).get(b);
      return b;
    }

    public X500Principal name()
    {
      int len = buffer.getShort(1) & 0xFFFF;
      byte[] b = new byte[len];
      ((ByteBuffer) buffer.duplicate().position(3)).get(b);
      return new X500Principal(b);
    }

    public IdentifierType type()
    {
      switch (buffer.get(0))
      {
        case 0: return IdentifierType.PRE_AGREED;
        case 1: return IdentifierType.KEY_SHA1_HASH;
        case 2: return IdentifierType.X509_NAME;
        case 3: return IdentifierType.CERT_SHA1_HASH;
      }

      throw new IllegalArgumentException("invalid IdentifierType");
    }

    public String toString()
    {
      return toString(null);
    }

    public String toString(String prefix)
    {
      StringWriter str = new StringWriter();
      PrintWriter out = new PrintWriter(str);
      if (prefix != null) out.print(prefix);
      out.println("struct {");
      if (prefix != null) out.print(prefix);
      out.print("  identifier_type = ");
      out.print(type());
      out.println(";");
      switch (type().getValue())
      {
        case 0: break;
        case 1:
        case 3:
          if (prefix != null) out.print(prefix);
          out.print("  sha1_hash = ");
          out.print(Util.toHexString(sha1Hash(), ':'));
          out.println(";");
          break;

        case 2:
          if (prefix != null) out.print(prefix);
          out.print("  name = ");
          out.print(name());
          out.println(";");
      }
      if (prefix != null) out.print(prefix);
      out.print("} TrustedAuthority;");
      return str.toString();
    }
  }

  public static enum IdentifierType
  {
    PRE_AGREED (0), KEY_SHA1_HASH (1), X509_NAME (2), CERT_SHA1_HASH (3);

    private final int value;

    private IdentifierType(final int value)
    {
      this.value = value;
    }

    public int getValue()
    {
      return value;
    }
  }
}
