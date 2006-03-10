/* DigestInputStream.java -- digesting input stream.
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

import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;

import gnu.java.security.hash.IMessageDigest;

final class DigestInputStream extends FilterInputStream
{

  // Fields.
  // -------------------------------------------------------------------------

  private IMessageDigest md5, sha;
  private boolean digesting;

  // Constructor.
  // -------------------------------------------------------------------------

  DigestInputStream(InputStream in, IMessageDigest md5, IMessageDigest sha)
  {
    super(in);
    if (md5 == null || sha == null)
      throw new NullPointerException();
    this.md5 = md5;
    this.sha = sha;
    digesting = true;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  void setDigesting(boolean digesting)
  {
    this.digesting = digesting;
  }

  public int read() throws IOException
  {
    int i = in.read();
    if (digesting && i != -1)
      {
        md5.update((byte) i);
        sha.update((byte) i);
      }
    return i;
  }

  public int read(byte[] buf) throws IOException
  {
    return read(buf, 0, buf.length);
  }

  public int read(byte[] buf, int off, int len) throws IOException
  {
    int ret = in.read(buf, off, len);
    if (digesting && ret != -1)
      {
        md5.update(buf, off, ret);
        sha.update(buf, off, ret);
      }
    return ret;
  }
}
