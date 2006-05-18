/* KeyImpl.java -- kerberos key implementation
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.security.auth.kerberos;

import java.io.Serializable;

import javax.crypto.SecretKey;

/**
 * Note that the name of this class is fixed by the serialization
 * spec, even though the class itself is not public.
 */
final class KeyImpl implements Serializable, SecretKey
{
  // Enable this when serialization works.
  // private static final long serialVersionUID = -7889313790214321193L;

  public String algorithm;
  public int type;
  public byte[] key;

  public KeyImpl(byte[] key, int type)
  {
    // From kerberos spec.
    if (type == 0)
      this.algorithm = null;
    else if (type == 1)
      this.algorithm = "DES";
    else
      this.algorithm = "FIXME";
    this.type = type;
    this.key = (byte[]) key.clone();
  }

  public KeyImpl(char[] passwd, String algo)
  {
    this.algorithm = (algo == null) ? "DES" : algo;
    this.type = 0; // FIXME
    this.key = null; // double FIXME
  }

  public String getAlgorithm()
  {
    return algorithm;
  }

  public byte[] getEncoded()
  {
    return key;
  }

  public String getFormat()
  {
    // FIXME.
    return null;
  }
}
