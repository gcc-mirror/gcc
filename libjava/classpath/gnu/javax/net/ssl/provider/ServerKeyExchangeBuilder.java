/* ServerKeyExchangeBuilder.java -- 
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


package gnu.javax.net.ssl.provider;

import java.nio.ByteBuffer;

/**
 * Builder for {@link ServerKeyExchange} objects.
 * 
 * @author Casey Marshall (csm@gnu.org)
 */
public class ServerKeyExchangeBuilder extends ServerKeyExchange
    implements Builder
{
  public ServerKeyExchangeBuilder(final CipherSuite suite)
  {
    super(ByteBuffer.allocate(1024), suite);
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.Builder#buffer()
   */
  public ByteBuffer buffer()
  {
    return ((ByteBuffer) buffer.duplicate().position(0).limit(length())).slice();
  }

  public void setParams(ByteBuffer params)
  {
    if (suite.keyExchangeAlgorithm() == KeyExchangeAlgorithm.NONE)
      throw new IllegalArgumentException("key exchange algorithm is none");
    ensureCapacity(params.remaining());
    buffer.duplicate().put(params);
  }
  
  public void setSignature(ByteBuffer signature)
  {
    if (suite.keyExchangeAlgorithm() == KeyExchangeAlgorithm.NONE)
      throw new IllegalArgumentException("key exchange algorithm is none");
    int paramsLen = params().length();
    ensureCapacity(paramsLen + signature.remaining());
    ((ByteBuffer) buffer.duplicate().position(paramsLen)).put(signature);
  }
  
  public void ensureCapacity(int capacity)
  {
    if (buffer.capacity() >= capacity)
      return;
    ByteBuffer newBuffer = ByteBuffer.allocate(capacity);
    newBuffer.duplicate().put(buffer);
    buffer = newBuffer;
  }
}
