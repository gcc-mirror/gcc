/* Gnu.java --- Gnu provider main class
   Copyright (C) 1999, 2002, 2003, 2005 Free Software Foundation, Inc.

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


package gnu.java.security.provider;

import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.Provider;

public final class Gnu
    extends Provider
{
  public Gnu()
  {
    super("GNU", 1.0,
          "GNU provider v1.0 implementing SHA-1, MD5, DSA, RSA, X.509 "
          + "Certificates and CRLs, PKIX certificate path validators, "
          + "Collection cert stores, Diffie-Hellman key agreement and "
          + "key pair generator");
    AccessController.doPrivileged (new PrivilegedAction()
    {
      public Object run()
      {
        // Note that all implementation class names are referenced by using
        // Class.getName(). That way when we staticly link the Gnu provider
        // we automatically get all the implementation classes.

        // Signature
        put("Signature.SHA160withDSS",
            gnu.java.security.jce.sig.SHA160withDSS.class.getName());
        put("Alg.Alias.Signature.SHA1withDSA", "SHA160withDSS");
        put("Alg.Alias.Signature.DSS", "SHA160withDSS");
        put("Alg.Alias.Signature.DSA", "SHA160withDSS");
        put("Alg.Alias.Signature.SHAwithDSA", "SHA160withDSS");
        put("Alg.Alias.Signature.DSAwithSHA", "SHA160withDSS");
        put("Alg.Alias.Signature.DSAwithSHA1", "SHA160withDSS");
        put("Alg.Alias.Signature.SHA/DSA", "SHA160withDSS");
        put("Alg.Alias.Signature.SHA-1/DSA", "SHA160withDSS");
        put("Alg.Alias.Signature.SHA1/DSA", "SHA160withDSS");
        put("Alg.Alias.Signature.OID.1.2.840.10040.4.3", "SHA160withDSS");
        put("Alg.Alias.Signature.1.2.840.10040.4.3", "SHA160withDSS");
        put("Alg.Alias.Signature.1.3.14.3.2.13", "SHA160withDSS");
        put("Alg.Alias.Signature.1.3.14.3.2.27", "SHA160withDSS");

        put("Signature.MD2withRSA",
            gnu.java.security.jce.sig.MD2withRSA.class.getName());
        put("Signature.MD2withRSA ImplementedIn", "Software");
        put("Alg.Alias.Signature.md2WithRSAEncryption", "MD2withRSA");
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.2", "MD2withRSA");
        put("Alg.Alias.Signature.1.2.840.113549.1.1.2", "MD2withRSA");

        put("Signature.MD5withRSA",
            gnu.java.security.jce.sig.MD5withRSA.class.getName());
        put("Signature.MD5withRSA ImplementedIn", "Software");
        put("Alg.Alias.Signature.md5WithRSAEncryption", "MD5withRSA");
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.4", "MD5withRSA");
        put("Alg.Alias.Signature.1.2.840.113549.1.1.4", "MD5withRSA");
        put("Alg.Alias.Signature.RSA", "MD5withRSA");

        put("Signature.SHA160withRSA",
            gnu.java.security.jce.sig.SHA160withRSA.class.getName());
        put("Signature.SHA160withRSA ImplementedIn", "Software");
        put("Alg.Alias.Signature.sha-1WithRSAEncryption", "SHA160withRSA");
        put("Alg.Alias.Signature.sha-160WithRSAEncryption", "SHA160withRSA");
        put("Alg.Alias.Signature.sha1WithRSAEncryption", "SHA160withRSA");
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.5", "SHA160withRSA");
        put("Alg.Alias.Signature.1.2.840.113549.1.1.5", "SHA160withRSA");
        put("Alg.Alias.Signature.SHA1withRSA", "SHA160withRSA");

        put("Signature.SHA256withRSA",
            gnu.java.security.jce.sig.SHA256withRSA.class.getName());
        put("Signature.SHA160withRSA ImplementedIn", "Software");
        put("Alg.Alias.Signature.sha256WithRSAEncryption", "SHA256withRSA");
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.11", "SHA256withRSA");
        put("Alg.Alias.Signature.1.2.840.113549.1.1.11", "SHA256withRSA");

        put("Signature.SHA384withRSA",
            gnu.java.security.jce.sig.SHA384withRSA.class.getName());
        put("Signature.SHA160withRSA ImplementedIn", "Software");
        put("Alg.Alias.Signature.sha384WithRSAEncryption", "SHA384withRSA");
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.12", "SHA384withRSA");
        put("Alg.Alias.Signature.1.2.840.113549.1.1.12", "SHA384withRSA");

        put("Signature.SHA512withRSA",
            gnu.java.security.jce.sig.SHA512withRSA.class.getName());
        put("Signature.SHA160withRSA ImplementedIn", "Software");
        put("Alg.Alias.Signature.sha512WithRSAEncryption", "SHA512withRSA");
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.13", "SHA512withRSA");
        put("Alg.Alias.Signature.1.2.840.113549.1.1.13", "SHA512withRSA");

        put("Signature.DSS/RAW",
            gnu.java.security.jce.sig.DSSRawSignatureSpi.class.getName());
        put("Signature.DSS/RAW KeySize", "1024");
        put("Signature.DSS/RAW ImplementedIn", "Software");

        put("Signature.RSA-PSS/RAW",
            gnu.java.security.jce.sig.RSAPSSRawSignatureSpi.class.getName());
        put("Signature.RSA-PSS/RAW KeySize", "1024");
        put("Signature.RSA-PSS/RAW ImplementedIn", "Software");

        // Key Pair Generator
        put("KeyPairGenerator.DSS",
            gnu.java.security.jce.sig.DSSKeyPairGeneratorSpi.class.getName());
        put("KeyPairGenerator.DSS KeySize", "1024");
        put("KeyPairGenerator.DSS ImplementedIn", "Software");
        put("Alg.Alias.KeyPairGenerator.DSA", "DSS");
        put("Alg.Alias.KeyPairGenerator.OID.1.2.840.10040.4.1", "DSS");
        put("Alg.Alias.KeyPairGenerator.1.2.840.10040.4.1", "DSS");
        put("Alg.Alias.KeyPairGenerator.1.3.14.3.2.12", "DSS");

        put("KeyPairGenerator.RSA",
            gnu.java.security.jce.sig.RSAKeyPairGeneratorSpi.class.getName());
        put("KeyPairGenerator.RSA KeySize", "1024");
        put("KeyPairGenerator.RSA ImplementedIn", "Software");

        // Key Factory
        put("KeyFactory.DSS",
            gnu.java.security.jce.sig.DSSKeyFactory.class.getName());
        put("Alg.Alias.KeyFactory.DSA", "DSS");
        put("Alg.Alias.KeyFactory.OID.1.2.840.10040.4.1", "DSS");
        put("Alg.Alias.KeyFactory.1.2.840.10040.4.1", "DSS");
        put("Alg.Alias.KeyFactory.1.3.14.3.2.12", "DSS");

        put("KeyFactory.RSA",
            gnu.java.security.jce.sig.RSAKeyFactory.class.getName());

        put("KeyFactory.Encoded",
            gnu.java.security.jce.sig.EncodedKeyFactory.class.getName());
        put("KeyFactory.Encoded ImplementedIn", "Software");
        put("Alg.Alias.KeyFactory.X.509", "Encoded");
        put("Alg.Alias.KeyFactory.X509", "Encoded");
        put("Alg.Alias.KeyFactory.PKCS#8", "Encoded");
        put("Alg.Alias.KeyFactory.PKCS8", "Encoded");

        put("MessageDigest.HAVAL",
            gnu.java.security.jce.hash.HavalSpi.class.getName());
        put("MessageDigest.HAVAL ImplementedIn", "Software");
        put("MessageDigest.MD2",
            gnu.java.security.jce.hash.MD2Spi.class.getName());
        put("MessageDigest.MD2 ImplementedIn", "Software");
        put("MessageDigest.MD4",
            gnu.java.security.jce.hash.MD4Spi.class.getName());
        put("MessageDigest.MD4 ImplementedIn", "Software");
        put("MessageDigest.MD5",
            gnu.java.security.jce.hash.MD5Spi.class.getName());
        put("MessageDigest.MD5 ImplementedIn", "Software");
        put("MessageDigest.RIPEMD128",
            gnu.java.security.jce.hash.RipeMD128Spi.class.getName());
        put("MessageDigest.RIPEMD128 ImplementedIn", "Software");
        put("MessageDigest.RIPEMD160",
            gnu.java.security.jce.hash.RipeMD160Spi.class.getName());
        put("MessageDigest.RIPEMD160 ImplementedIn", "Software");
        put("MessageDigest.SHA-160",
            gnu.java.security.jce.hash.Sha160Spi.class.getName());
        put("MessageDigest.SHA-160 ImplementedIn", "Software");
        put("MessageDigest.SHA-256",
            gnu.java.security.jce.hash.Sha256Spi.class.getName());
        put("MessageDigest.SHA-256 ImplementedIn", "Software");
        put("MessageDigest.SHA-384",
            gnu.java.security.jce.hash.Sha384Spi.class.getName());
        put("MessageDigest.SHA-384 ImplementedIn", "Software");
        put("MessageDigest.SHA-512",
            gnu.java.security.jce.hash.Sha512Spi.class.getName());
        put("MessageDigest.SHA-512 ImplementedIn", "Software");
        put("MessageDigest.TIGER",
            gnu.java.security.jce.hash.TigerSpi.class.getName());
        put("MessageDigest.TIGER ImplementedIn", "Software");
        put("MessageDigest.WHIRLPOOL",
            gnu.java.security.jce.hash.WhirlpoolSpi.class.getName());
        put("MessageDigest.WHIRLPOOL ImplementedIn", "Software");

        put("Alg.Alias.MessageDigest.SHS", "SHA-160");
        put("Alg.Alias.MessageDigest.SHA", "SHA-160");
        put("Alg.Alias.MessageDigest.SHA1", "SHA-160");
        put("Alg.Alias.MessageDigest.SHA-1", "SHA-160");
        put("Alg.Alias.MessageDigest.SHA2-256", "SHA-256");
        put("Alg.Alias.MessageDigest.SHA2-384", "SHA-384");
        put("Alg.Alias.MessageDigest.SHA2-512", "SHA-512");
        put("Alg.Alias.MessageDigest.SHA256", "SHA-256");
        put("Alg.Alias.MessageDigest.SHA384", "SHA-384");
        put("Alg.Alias.MessageDigest.SHA512", "SHA-512");
        put("Alg.Alias.MessageDigest.RIPEMD-160", "RIPEMD160");
        put("Alg.Alias.MessageDigest.RIPEMD-128", "RIPEMD128");
        put("Alg.Alias.MessageDigest.OID.1.2.840.11359.2.2", "MD2");
        put("Alg.Alias.MessageDigest.1.2.840.11359.2.2", "MD2");
        put("Alg.Alias.MessageDigest.OID.1.2.840.11359.2.5", "MD5");
        put("Alg.Alias.MessageDigest.1.2.840.11359.2.5", "MD5");
        put("Alg.Alias.MessageDigest.OID.1.3.14.3.2.26", "SHA1");
        put("Alg.Alias.MessageDigest.1.3.14.3.2.26", "SHA1");

        // Algorithm Parameters
        put("AlgorithmParameters.DSS",
            gnu.java.security.jce.sig.DSSParameters.class.getName());
        put("Alg.Alias.AlgorithmParameters.DSA", "DSS");
        put("Alg.Alias.AlgorithmParameters.SHAwithDSA", "DSS");
        put("Alg.Alias.AlgorithmParameters.OID.1.2.840.10040.4.3", "DSS");
        put("Alg.Alias.AlgorithmParameters.1.2.840.10040.4.3", "DSS");

        // Algorithm Parameter Generator
        put("AlgorithmParameterGenerator.DSA",
            gnu.java.security.jce.sig.DSSParametersGenerator.class.getName());
        put("Alg.Alias.AlgorithmParameterGenerator.DSA", "DSS");

        // SecureRandom
        put("SecureRandom.SHA1PRNG",
            gnu.java.security.jce.prng.Sha160RandomSpi.class.getName());

        put("SecureRandom.MD2PRNG",
            gnu.java.security.jce.prng.MD2RandomSpi.class.getName());
        put("SecureRandom.MD2PRNG ImplementedIn", "Software");
        put("SecureRandom.MD4PRNG",
            gnu.java.security.jce.prng.MD4RandomSpi.class.getName());
        put("SecureRandom.MD4PRNG ImplementedIn", "Software");
        put("SecureRandom.MD5PRNG",
            gnu.java.security.jce.prng.MD5RandomSpi.class.getName());
        put("SecureRandom.MD5PRNG ImplementedIn", "Software");
        put("SecureRandom.RIPEMD128PRNG",
            gnu.java.security.jce.prng.RipeMD128RandomSpi.class.getName());
        put("SecureRandom.RIPEMD128PRNG ImplementedIn", "Software");
        put("SecureRandom.RIPEMD160PRNG",
            gnu.java.security.jce.prng.RipeMD160RandomSpi.class.getName());
        put("SecureRandom.RIPEMD160PRNG ImplementedIn", "Software");
        put("SecureRandom.SHA-160PRNG",
            gnu.java.security.jce.prng.Sha160RandomSpi.class.getName());
        put("SecureRandom.SHA-160PRNG ImplementedIn", "Software");
        put("SecureRandom.SHA-256PRNG",
            gnu.java.security.jce.prng.Sha256RandomSpi.class.getName());
        put("SecureRandom.SHA-256PRNG ImplementedIn", "Software");
        put("SecureRandom.SHA-384PRNG",
            gnu.java.security.jce.prng.Sha384RandomSpi.class.getName());
        put("SecureRandom.SHA-384PRNG ImplementedIn", "Software");
        put("SecureRandom.SHA-512PRNG",
            gnu.java.security.jce.prng.Sha512RandomSpi.class.getName());
        put("SecureRandom.SHA-512PRNG ImplementedIn", "Software");
        put("SecureRandom.TIGERPRNG",
            gnu.java.security.jce.prng.TigerRandomSpi.class.getName());
        put("SecureRandom.TIGERPRNG ImplementedIn", "Software");
        put("SecureRandom.HAVALPRNG",
            gnu.java.security.jce.prng.HavalRandomSpi.class.getName());
        put("SecureRandom.HAVALPRNG ImplementedIn", "Software");
        put("SecureRandom.WHIRLPOOLPRNG",
            gnu.java.security.jce.prng.WhirlpoolRandomSpi.class.getName());
        put("SecureRandom.WHIRLPOOLPRNG ImplementedIn", "Software");

        put("Alg.Alias.SecureRandom.SHA-1PRNG", "SHA-160PRNG");
        put("Alg.Alias.SecureRandom.SHA1PRNG", "SHA-160PRNG");
        put("Alg.Alias.SecureRandom.SHAPRNG", "SHA-160PRNG");
        put("Alg.Alias.SecureRandom.SHA-256PRNG", "SHA-256PRNG");
        put("Alg.Alias.SecureRandom.SHA-2-1PRNG", "SHA-256PRNG");
        put("Alg.Alias.SecureRandom.SHA-384PRNG", "SHA-384PRNG");
        put("Alg.Alias.SecureRandom.SHA-2-2PRNG", "SHA-384PRNG");
        put("Alg.Alias.SecureRandom.SHA-512PRNG", "SHA-512PRNG");
        put("Alg.Alias.SecureRandom.SHA-2-3PRNG", "SHA-512PRNG");

        // CertificateFactory
        put("CertificateFactory.X509", X509CertificateFactory.class.getName());
        put("CertificateFactory.X509 ImplementedIn", "Software");
        put("Alg.Alias.CertificateFactory.X.509", "X509");

        // CertPathValidator
        put("CertPathValidator.PKIX", PKIXCertPathValidatorImpl.class.getName());
        put("CertPathValidator.PKIX ImplementedIn", "Software");

        // CertStore
        put("CertStore.Collection", CollectionCertStoreImpl.class.getName());

        return null;
      }
    });
  }
}
