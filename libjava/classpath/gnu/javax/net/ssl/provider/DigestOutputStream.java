/* DigestOutputStream.java -- digesting output stream.
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

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import gnu.java.security.hash.IMessageDigest;

final class DigestOutputStream extends FilterOutputStream
{

  // Fields.
  // -------------------------------------------------------------------------

  private IMessageDigest md5, sha;
  private boolean digesting;

  // Constructor.
  // -------------------------------------------------------------------------

  DigestOutputStream(OutputStream out, IMessageDigest md5, IMessageDigest sha)
  {
    super(out);
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

  public void write(int b) throws IOException
  {
    if (digesting)
      {
        md5.update((byte) b);
        sha.update((byte) b);
      }
    out.write(b);
  }

  public void write(byte[] buf) throws IOException
  {
    write(buf, 0, buf.length);
  }

  public void write(byte[] buf, int off, int len) throws IOException
  {
    if (buf == null)
      {
        throw new NullPointerException();
      }
    if (off < 0 || len < 0 || off+len > buf.length)
      {
        throw new ArrayIndexOutOfBoundsException();
      }
    if (digesting)
      {
        md5.update(buf, off, len);
        sha.update(buf, off, len);
      }
    out.write(buf, off, len);
  }
}
