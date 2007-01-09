/* CertificateStatusRequest.java --
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
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * <pre>
struct {
  CertificateStatusType status_type;
  select (status_type) {
    case ocsp: OCSPStatusRequest;
  } request;
} CertificateStatusRequest;

enum { ocsp(1), (255) } CertificateStatusType;

struct {
  ResponderID responder_id_list&lt;0..2^16-1&gt;;
  Extensions  request_extensions;
} OCSPStatusRequest;

opaque ResponderID&lt;1..2^16-1&gt;;
opaque Extensions&lt;0..2^16-1&gt;;</pre>
 *
 * @author csm
 */
public class CertificateStatusRequest extends Value implements Iterable<byte[]>
{
  private ByteBuffer buffer;
  
  public CertificateStatusRequest(final ByteBuffer buffer)
  {
    this.buffer = buffer;
  }
  
  public CertificateStatusRequest(CertificateStatusType type,
                                  List<byte[]> responderIdList,
                                  byte[] requestExtensions)
  {
    if (type != CertificateStatusType.OCSP)
      throw new IllegalArgumentException();
    int length = 3;
    int idsLength = 0;
    for (byte[] responderId : responderIdList)
      {
        length += 2 + responderId.length;
        idsLength += 2 + responderId.length;
      }
    length += 2 + requestExtensions.length;
    buffer = ByteBuffer.allocate(length);
    buffer.put((byte) 1);
    buffer.putShort((short) idsLength);
    for (byte[] responderId : responderIdList)
      buffer.putShort((short) responderId.length).put(responderId);
    buffer.putShort((short) requestExtensions.length);
    buffer.put(requestExtensions);
    buffer.rewind();
  }

  public int length()
  {
    int l = 3 + (buffer.getShort(1) & 0xFFFF);
    return l + (buffer.getShort(l) & 0xFFFF) + 2;
  }
  
  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().limit(length());
  }
  
  public CertificateStatusType statusType()
  {
    int x = buffer.get(0) & 0xFF;
    if (x == 1)
      return CertificateStatusType.OCSP;
    throw new IllegalArgumentException ("invalid type: " + x);
  }

  public int size()
  {
    int len = buffer.getShort(1) & 0xFFFF;
    int n = 0;
    for (int i = 3; i < len; )
      {
        int l = buffer.getShort(i);
        i += l + 2;
        n++;
      }
    return n;
  }
  
  public byte[] responderId(int index)
  {
    int len = buffer.getShort(1) & 0xFFFF;
    int n = 0;
    int i = 3;
    while (i < len && n <= index)
      {
        int l = buffer.getShort(i) & 0xFFFF;
        if (n == index)
          {
            byte[] b = new byte[l];
            ((ByteBuffer) buffer.duplicate().position(i+2)).get(b);
            return b;
          }
        i += l + 2;
        n++;
      }
    throw new IndexOutOfBoundsException();
  }
  
  public byte[] requestExtensions()
  {
    int l = 2 + (buffer.getShort(0) & 0xFFFF);
    int ll = buffer.getShort(l) & 0xFFFF;
    byte[] b = new byte[ll];
    ((ByteBuffer) buffer.duplicate().position(ll+2)).get(b);
    return b;
  }
  
  public void setStatusType(CertificateStatusType type)
  {
    buffer.put(0, (byte) type.value);
  }
  
  public void setRequestIdListLength(int newLength)
  {
    if (newLength < 0 || newLength > 0xFFFF)
      throw new IllegalArgumentException("length out of range");
    buffer.putShort(1, (short) newLength);
  }
  
  public void putRequestId(int index, byte[] id)
  {
    if (id.length > 0xFFFF)
      throw new IllegalArgumentException("request ID too large");
    int len = buffer.getShort(1) & 0xFFFF;
    int n = 0;
    int i = 3;
    while (i < len && n < index)
      {
        int l = buffer.getShort(i) & 0xFFFF;
        i += l + 2;
        n++;
      }
    if (n < index)
      throw new IndexOutOfBoundsException();
    buffer.putShort(i, (short) id.length);
    ((ByteBuffer) buffer.duplicate().position(i)).put(id);
  }
  
  public void setRequestExtensions(int index, byte[] ext)
  {
    if (ext.length > 0xFFFF)
      throw new IllegalArgumentException("exceptions too large");
    int off = 3 + (buffer.getShort(1) & 0xFFFF);
    buffer.putShort(off, (short) ext.length);
    ((ByteBuffer) buffer.duplicate().position(off+2)).put(ext);
  }
  
  public Iterator<byte[]> iterator()
  {
    return new ResponderIdIterator();
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
    out.print("  status_type = ");
    out.print(statusType());
    out.println(";");
    String subprefix = "    ";
    if (prefix != null) subprefix = prefix + subprefix;
    if (prefix != null) out.print(prefix);
    out.println("  responder_id_list = {");
    for (byte[] b : this)
      out.print(Util.hexDump(b, subprefix));
    if (prefix != null) out.print(prefix);
    out.println("  };");
    if (prefix != null) out.print(prefix);
    out.println("  request_extensions =");
    out.print(Util.hexDump(requestExtensions(), subprefix));
    if (prefix != null) out.print(prefix);
    out.print("} CertificateStatus;");
    return str.toString();
  }
  
  public class ResponderIdIterator implements Iterator<byte[]>
  {
    private int index;
    
    public ResponderIdIterator()
    {
      index = 0;
    }
    
    public byte[] next() throws NoSuchElementException
    {
      try
        {
          return responderId(index++);
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
}
