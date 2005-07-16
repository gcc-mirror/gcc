/* DummyMessageDigest.java - Wrapper for MessageDigestSpi
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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

package java.security;

final class DummyMessageDigest extends MessageDigest
{
  private MessageDigestSpi mdSpi = null;

  public DummyMessageDigest(MessageDigestSpi mdSpi, String algorithm)
  {
    super(algorithm);
    this.mdSpi = mdSpi;
  }

  public Object clone() throws CloneNotSupportedException
  {
    MessageDigest result = new DummyMessageDigest
        ((MessageDigestSpi) mdSpi.clone(), this.getAlgorithm());
    result.provider = this.getProvider();
    return result;
  }

  // java.security.MessageDigestSpi abstract methods implementation ---------

  public byte[] engineDigest()
  {
    return mdSpi.engineDigest();
  }

  public int engineDigest(byte[] buf, int offset, int len)
    throws DigestException
  {
    return mdSpi.engineDigest(buf, offset, len);
  }

  public int engineGetDigestLength()
  {
    return mdSpi.engineGetDigestLength();
  }

  public void engineReset()
  {
    mdSpi.engineReset();
  }

  public void engineUpdate(byte input)
  {
    mdSpi.engineUpdate(input);
  }

  public void engineUpdate(byte[] input, int offset, int len)
  {
    mdSpi.engineUpdate(input, offset, len);
  }
}
