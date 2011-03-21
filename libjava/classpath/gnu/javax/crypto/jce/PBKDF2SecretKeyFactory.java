/* PBKDF2SecretKeyFactory.java --
   Copyright (C) 2003, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce;

import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

import java.util.HashMap;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactorySpi;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import gnu.javax.crypto.prng.IPBE;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.javax.crypto.prng.PRNGFactory;

public abstract class PBKDF2SecretKeyFactory
    extends SecretKeyFactorySpi
{
  protected String macName;
  private static final int DEFAULT_ITERATION_COUNT = 1000;
  private static final int DEFAULT_KEY_LEN = 32;

  protected PBKDF2SecretKeyFactory(String macName)
  {
    this.macName = macName;
  }

  protected SecretKey engineGenerateSecret(KeySpec spec)
      throws InvalidKeySpecException
  {
    if (! (spec instanceof PBEKeySpec))
      throw new InvalidKeySpecException("not a PBEKeySpec");
    IRandom kdf = PRNGFactory.getInstance("PBKDF2-" + macName);
    HashMap attr = new HashMap();
    attr.put(IPBE.PASSWORD, ((PBEKeySpec) spec).getPassword());
    byte[] salt = ((PBEKeySpec) spec).getSalt();
    if (salt == null)
      salt = new byte[0];
    attr.put(IPBE.SALT, salt);
    int ic = ((PBEKeySpec) spec).getIterationCount();
    if (ic <= 0)
      ic = DEFAULT_ITERATION_COUNT;
    attr.put(IPBE.ITERATION_COUNT, Integer.valueOf(ic));
    kdf.init(attr);
    int len = ((PBEKeySpec) spec).getKeyLength();
    if (len <= 0)
      len = DEFAULT_KEY_LEN;
    byte[] dk = new byte[len];
    try
      {
        kdf.nextBytes(dk, 0, len);
      }
    catch (LimitReachedException lre)
      {
        throw new IllegalArgumentException(lre.toString());
      }
    return new SecretKeySpec(dk, "PBKDF2");
  }

  protected KeySpec engineGetKeySpec(SecretKey key, Class clazz)
      throws InvalidKeySpecException
  {
    throw new InvalidKeySpecException("not supported");
  }

  protected SecretKey engineTranslateKey(SecretKey key)
  {
    return new SecretKeySpec(key.getEncoded(), key.getAlgorithm());
  }

  public static class HMacHaval
      extends PBKDF2SecretKeyFactory
  {
    public HMacHaval()
    {
      super("HMAC-HAVAL");
    }
  }

  public static class HMacMD2
      extends PBKDF2SecretKeyFactory
  {
    public HMacMD2()
    {
      super("HMAC-MD2");
    }
  }

  public static class HMacMD4
      extends PBKDF2SecretKeyFactory
  {
    public HMacMD4()
    {
      super("HMAC-MD4");
    }
  }

  public static class HMacMD5
      extends PBKDF2SecretKeyFactory
  {
    public HMacMD5()
    {
      super("HMAC-MD5");
    }
  }

  public static class HMacRipeMD128
      extends PBKDF2SecretKeyFactory
  {
    public HMacRipeMD128()
    {
      super("HMAC-RIPEMD128");
    }
  }

  public static class HMacRipeMD160
      extends PBKDF2SecretKeyFactory
  {
    public HMacRipeMD160()
    {
      super("HMAC-RIPEMD160");
    }
  }

  public static class HMacSHA1
      extends PBKDF2SecretKeyFactory
  {
    public HMacSHA1()
    {
      super("HMAC-SHA1");
    }
  }

  public static class HMacSHA256
      extends PBKDF2SecretKeyFactory
  {
    public HMacSHA256()
    {
      super("HMAC-SHA256");
    }
  }

  public static class HMacSHA384
      extends PBKDF2SecretKeyFactory
  {
    public HMacSHA384()
    {
      super("HMAC-SHA384");
    }
  }

  public static class HMacSHA512
      extends PBKDF2SecretKeyFactory
  {
    public HMacSHA512()
    {
      super("HMAC-SHA512");
    }
  }

  public static class HMacTiger
      extends PBKDF2SecretKeyFactory
  {
    public HMacTiger()
    {
      super("HMAC-TIGER");
    }
  }

  public static class HMacWhirlpool
      extends PBKDF2SecretKeyFactory
  {
    public HMacWhirlpool()
    {
      super("HMAC-WHIRLPOOL");
    }
  }
}
