/* PBES2.java --
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


package gnu.javax.crypto.jce.cipher;

import gnu.javax.crypto.prng.IPBE;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.javax.crypto.prng.PRNGFactory;

import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;
import java.util.HashMap;

import javax.crypto.interfaces.PBEKey;
import javax.crypto.spec.SecretKeySpec;

/**
 */
public abstract class PBES2
    extends CipherAdapter
{
  /** The HMac (PRF) algorithm name. */
  protected String macName;

  protected PBES2(String cipherName, int blockLen, String macName)
  {
    super(cipherName, blockLen);
    this.macName = macName;
  }

  protected PBES2(String cipherName, String macName)
  {
    super(cipherName);
    this.macName = macName;
  }

  protected void engineInit(int opmode, Key key, SecureRandom random)
      throws InvalidKeyException
  {
    if (! (key instanceof PBEKey))
      throw new InvalidKeyException("not a PBE key");
    super.engineInit(opmode, genkey((PBEKey) key), random);
  }

  protected void engineInit(int opmode, Key key, AlgorithmParameterSpec params,
                            SecureRandom random) throws InvalidKeyException,
      InvalidAlgorithmParameterException
  {
    if (! (key instanceof PBEKey))
      throw new InvalidKeyException("not a PBE key");
    super.engineInit(opmode, genkey((PBEKey) key), params, random);
  }

  protected void engineInit(int opmode, Key key, AlgorithmParameters params,
                            SecureRandom random) throws InvalidKeyException,
      InvalidAlgorithmParameterException
  {
    if (! (key instanceof PBEKey))
      throw new InvalidKeyException("not a PBE key");
    super.engineInit(opmode, genkey((PBEKey) key), params, random);
  }

  private SecretKeySpec genkey(PBEKey key) throws InvalidKeyException
  {
    IRandom kdf = PRNGFactory.getInstance("PBKDF2-" + macName);
    if (kdf == null)
      throw new IllegalArgumentException("no such KDF: PBKDF2-" + macName);
    HashMap attrib = new HashMap();
    attrib.put(IPBE.ITERATION_COUNT, Integer.valueOf(key.getIterationCount()));
    attrib.put(IPBE.PASSWORD, key.getPassword());
    attrib.put(IPBE.SALT, key.getSalt());
    try
      {
        kdf.init(attrib);
      }
    catch (IllegalArgumentException iae)
      {
        throw new InvalidKeyException(iae.toString());
      }
    byte[] dk = new byte[mode.defaultKeySize()];
    try
      {
        kdf.nextBytes(dk, 0, dk.length);
      }
    catch (LimitReachedException shouldNotHappen)
      {
        throw new Error(String.valueOf(shouldNotHappen));
      }
    return new SecretKeySpec(dk, cipher.name());
  }

  public static class HMacSHA1
      extends PBES2
  {
    public HMacSHA1(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-SHA1");
    }

    public HMacSHA1(String cipher)
    {
      super(cipher, "HMAC-SHA1");
    }

    public static class AES
        extends HMacSHA1
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacSHA1
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacSHA1
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacSHA1
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacSHA1
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacSHA1
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacSHA1
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacSHA1
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacSHA1
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacSHA1
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacMD5
      extends PBES2
  {
    public HMacMD5(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-MD5");
    }

    public HMacMD5(String cipher)
    {
      super(cipher, "HMAC-MD5");
    }

    public static class AES
        extends HMacMD5
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacMD5
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacMD5
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacMD5
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacMD5
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacMD5
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacMD5
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacMD5
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacMD5
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacMD5
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacMD2
      extends PBES2
  {
    public HMacMD2(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-MD2");
    }

    public HMacMD2(String cipher)
    {
      super(cipher, "HMAC-MD2");
    }

    public static class AES
        extends HMacMD2
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacMD2
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacMD2
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacMD2
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacMD2
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacMD2
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacMD2
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacMD2
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacMD2
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacMD2
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacMD4
      extends PBES2
  {
    public HMacMD4(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-MD4");
    }

    public HMacMD4(String cipher)
    {
      super(cipher, "HMAC-MD4");
    }

    public static class AES
        extends HMacMD4
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacMD4
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacMD4
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacMD4
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacMD4
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacMD4
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacMD4
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacMD4
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacMD4
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacMD4
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacHaval
      extends PBES2
  {
    public HMacHaval(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-HAVAL");
    }

    public HMacHaval(String cipher)
    {
      super(cipher, "HMAC-HAVAL");
    }

    public static class AES
        extends HMacHaval
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacHaval
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacHaval
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacHaval
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacHaval
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacHaval
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacHaval
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacHaval
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacHaval
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacHaval
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacRipeMD128
      extends PBES2
  {
    public HMacRipeMD128(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-RIPEMD128");
    }

    public HMacRipeMD128(String cipher)
    {
      super(cipher, "HMAC-RIPEMD128");
    }

    public static class AES
        extends HMacRipeMD128
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacRipeMD128
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacRipeMD128
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacRipeMD128
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacRipeMD128
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacRipeMD128
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacRipeMD128
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacRipeMD128
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacRipeMD128
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacRipeMD128
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacRipeMD160
      extends PBES2
  {
    public HMacRipeMD160(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-RIPEMD160");
    }

    public HMacRipeMD160(String cipher)
    {
      super(cipher, "HMAC-RIPEMD160");
    }

    public static class AES
        extends HMacRipeMD160
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacRipeMD160
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacRipeMD160
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacRipeMD160
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacRipeMD160
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacRipeMD160
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacRipeMD160
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacRipeMD160
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacRipeMD160
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacRipeMD160
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacSHA256
      extends PBES2
  {
    public HMacSHA256(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-SHA-256");
    }

    public HMacSHA256(String cipher)
    {
      super(cipher, "HMAC-SHA-256");
    }

    public static class AES
        extends HMacSHA256
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacSHA256
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacSHA256
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacSHA256
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacSHA256
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacSHA256
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacSHA256
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacSHA256
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacSHA256
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacSHA256
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacSHA384
      extends PBES2
  {
    public HMacSHA384(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-SHA-384");
    }

    public HMacSHA384(String cipher)
    {
      super(cipher, "HMAC-SHA-384");
    }

    public static class AES
        extends HMacSHA384
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacSHA384
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacSHA384
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacSHA384
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacSHA384
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacSHA384
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacSHA384
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacSHA384
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacSHA384
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacSHA384
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacSHA512
      extends PBES2
  {
    public HMacSHA512(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-SHA-512");
    }

    public HMacSHA512(String cipher)
    {
      super(cipher, "HMAC-SHA-512");
    }

    public static class AES
        extends HMacSHA512
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacSHA512
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacSHA512
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacSHA512
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacSHA512
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacSHA512
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacSHA512
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacSHA512
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacSHA512
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacSHA512
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacTiger
      extends PBES2
  {
    public HMacTiger(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-TIGER");
    }

    public HMacTiger(String cipher)
    {
      super(cipher, "HMAC-TIGER");
    }

    public static class AES
        extends HMacTiger
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacTiger
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacTiger
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacTiger
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacTiger
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacTiger
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacTiger
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacTiger
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacTiger
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacTiger
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }

  public static class HMacWhirlpool
      extends PBES2
  {
    public HMacWhirlpool(String cipher, int blockLen)
    {
      super(cipher, blockLen, "HMAC-WHIRLPOOL");
    }

    public HMacWhirlpool(String cipher)
    {
      super(cipher, "HMAC-WHIRLPOOL");
    }

    public static class AES
        extends HMacWhirlpool
    {
      public AES()
      {
        super("AES");
      }
    }

    public static class Anubis
        extends HMacWhirlpool
    {
      public Anubis()
      {
        super("Anubis");
      }
    }

    public static class Blowfish
        extends HMacWhirlpool
    {
      public Blowfish()
      {
        super("Blowfish");
      }
    }

    public static class Cast5
        extends HMacWhirlpool
    {
      public Cast5()
      {
        super("Cast5");
      }
    }

    public static class DES
        extends HMacWhirlpool
    {
      public DES()
      {
        super("DES");
      }
    }

    public static class Khazad
        extends HMacWhirlpool
    {
      public Khazad()
      {
        super("Khazad");
      }
    }

    public static class Serpent
        extends HMacWhirlpool
    {
      public Serpent()
      {
        super("Serpent");
      }
    }

    public static class Square
        extends HMacWhirlpool
    {
      public Square()
      {
        super("Square");
      }
    }

    public static class TripleDES
        extends HMacWhirlpool
    {
      public TripleDES()
      {
        super("TripleDES");
      }
    }

    public static class Twofish
        extends HMacWhirlpool
    {
      public Twofish()
      {
        super("Twofish");
      }
    }
  }
}
