/* ServerNameList.java --
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
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * The ServerName extension.
 *
 * <pre>
 struct {
   NameType name_type;
   select (name_type) {
     case host_name: HostName;
   } name;
} ServerName;

enum {
  host_name(0), (255)
} NameType;

opaque HostName<1..2^16-1>;

struct {
  ServerName server_name_list<1..2^16-1>
} ServerNameList;</pre>
 *
 * <p><b>Implementation note: this class does not currently contain a
 * <code>set</code> method. If you are modifying this list, then use the
 * {@link #get(int)} method, and modify the returned {@link ServerName}.
 *
 * @author csm
 */
public class ServerNameList extends Value implements Iterable<ServerNameList.ServerName>
{
  private ByteBuffer buffer;

  public ServerNameList (final ByteBuffer buffer)
  {
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
  }

  public ServerNameList(List<ServerName> names)
  {
    int length = 2;
    for (ServerName name : names)
      length += name.length();
    buffer = ByteBuffer.allocate(length);
    buffer.putShort((short) (length - 2));
    for (ServerName name : names)
      buffer.put(name.buffer());
    buffer.rewind();
  }

  public int length()
  {
    return (buffer.getShort(0) & 0xFFFF) + 2;
  }

  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().limit(length());
  }

  public int size()
  {
    int n = 0;
    final int len = length();
    for (int i = 2; i < len; )
      {
        int l = buffer.getShort(i+1);
        i += l + 3;
        n++;
      }
    return n;
  }

  public ServerName get (int index)
  {
    final int len = length();
    if (len == 0)
      throw new IndexOutOfBoundsException("0; " + index);
    int n = 0;
    int i;
    int l = buffer.getShort(3);
    for (i = 2; i < len && n < index; )
      {
        l = buffer.getShort(i+1);
        i += l + 3;
        n++;
      }
    if (n < index)
      throw new IndexOutOfBoundsException(n + "; " + index);
    ByteBuffer buf = ((ByteBuffer) buffer.duplicate().position(i).limit(i+l+3)).slice();
    return new ServerName (buf);
  }

  public void setLength(final int newLength)
  {
    if (newLength < 0 || newLength > 65535)
      throw new IllegalArgumentException("length must be between 0 and 65535");
    buffer.putShort(0, (short) newLength);
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
    out.println ("ServerNameList {");
    String subprefix = "  ";
    if (prefix != null)
      subprefix = prefix + subprefix;
    for (ServerName name : this)
      {
        out.println (name.toString(subprefix));
      }
    if (prefix != null) out.print(prefix);
    out.print ("};");
    return str.toString();
  }

  public java.util.Iterator<ServerName> iterator()
  {
    return new Iterator();
  }

  public class Iterator implements java.util.Iterator<ServerName>
  {
    private int index;

    public Iterator()
    {
      index = 0;
    }

    public boolean hasNext()
    {
      return index < size();
    }

    public ServerName next() throws NoSuchElementException
    {
      try
        {
          return get (index++);
        }
      catch (IndexOutOfBoundsException ioobe)
        {
          throw new NoSuchElementException();
        }
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }
  }

  public static class ServerName implements Constructed
  {
    private ByteBuffer buffer;

    public ServerName(final ByteBuffer buffer)
    {
      this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
    }

    public ServerName(NameType type, String name)
    {
      CharsetEncoder utf8 = Charset.forName("UTF-8").newEncoder();
      ByteBuffer nameBuf = null;
      try
        {
          nameBuf = utf8.encode(CharBuffer.wrap(name));
        }
      catch (CharacterCodingException cce)
        {
          // We don't expect this to happen; it's UTF-8.
          throw new IllegalArgumentException(cce);
        }
      int length = 3 + nameBuf.remaining();
      buffer = ByteBuffer.allocate(length);
      buffer.put((byte) type.getValue());
      buffer.putShort((short) (length - 3));
      buffer.put(nameBuf);
      buffer.rewind();
    }

    public int length()
    {
      return (buffer.getShort(1) & 0xFFFF) + 3;
    }

    public ByteBuffer buffer()
    {
      return (ByteBuffer) buffer.duplicate().limit(length());
    }

    public NameType type()
    {
      int v = (buffer.get(0) & 0xFF);
      if (v == 0)
        {
          return NameType.HOST_NAME;
        }
      throw new IllegalArgumentException ("illegal name type: " + v);
    }

    public String name()
    {
      int len = length();
      Charset cs = Charset.forName ("UTF-8");
      return cs.decode(((ByteBuffer) buffer.duplicate().position(3).limit(len))).toString();
    }

    public String toString()
    {
      return toString (null);
    }

    public String toString(String prefix)
    {
      StringWriter str = new StringWriter();
      PrintWriter out = new PrintWriter(str);
      if (prefix != null) out.print (prefix);
      out.println ("struct {");
      if (prefix != null) out.print (prefix);
      out.print ("  name_type = ");
      out.print (type());
      out.println (";");
      if (prefix != null) out.print (prefix);
      out.print ("  server_name = ");
      out.print (name());
      out.println (";");
      if (prefix != null) out.print (prefix);
      out.print ("} ServerName;");
      return str.toString();
    }
  }

  public static enum NameType
  {
    HOST_NAME (0);

    private final int value;

    private NameType (int value)
    {
      this.value = value;
    }

    public int getValue()
    {
      return value;
    }
  }
}
