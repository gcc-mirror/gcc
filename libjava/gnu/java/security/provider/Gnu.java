/* Gnu.java --- Gnu provider main class
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
import java.security.Provider;

public final class Gnu extends Provider
{
  public Gnu()
  {
    super( "GNU", 1.0, "GNU provider v1.0 implementing SHA-1, MD5, DSA");

    // Note that all implementation class names are referenced by using
    // Class.getName(). That way when we staticly link the Gnu provider
    // we automatically get all the implementation classes.

    // Signature
    put("Signature.SHA1withDSA",
	gnu.java.security.provider.DSASignature.class.getName());

    put("Alg.Alias.Signature.DSS", "SHA1withDSA");
    put("Alg.Alias.Signature.DSA", "SHA1withDSA");
    put("Alg.Alias.Signature.SHAwithDSA", "SHA1withDSA");
    put("Alg.Alias.Signature.DSAwithSHA", "SHA1withDSA");
    put("Alg.Alias.Signature.DSAwithSHA1", "SHA1withDSA");
    put("Alg.Alias.Signature.SHA/DSA", "SHA1withDSA");
    put("Alg.Alias.Signature.SHA-1/DSA", "SHA1withDSA");
    put("Alg.Alias.Signature.SHA1/DSA", "SHA1withDSA");
    put("Alg.Alias.Signature.OID.1.2.840.10040.4.3", "SHA1withDSA");
    put("Alg.Alias.Signature.1.2.840.10040.4.3", "SHA1withDSA");
    put("Alg.Alias.Signature.1.3.14.3.2.13", "SHA1withDSA");
    put("Alg.Alias.Signature.1.3.14.3.2.27", "SHA1withDSA");

    // Key Pair Generator
    put("KeyPairGenerator.DSA",
	gnu.java.security.provider.DSAKeyPairGenerator.class.getName());

    put("Alg.Alias.KeyPairGenerator.OID.1.2.840.10040.4.1", "DSA");
    put("Alg.Alias.KeyPairGenerator.1.2.840.10040.4.1", "DSA");
    put("Alg.Alias.KeyPairGenerator.1.3.14.3.2.12", "DSA");

    // Message Digests
    put("MessageDigest.SHA", gnu.java.security.provider.SHA.class.getName());
    put("MessageDigest.MD5", gnu.java.security.provider.MD5.class.getName());

    // Format "Alias", "Actual Name"
    put("Alg.Alias.MessageDigest.SHA1", "SHA");
    put("Alg.Alias.MessageDigest.SHA-1", "SHA");

    // Algorithm Parameters
    put("AlgorithmParameters.DSA",
	gnu.java.security.provider.DSAParameters.class.getName());

    // Algorithm Parameter Generator
    put("AlgorithmParameterGenerator.DSA",
     gnu.java.security.provider.DSAParameterGenerator.class.getName());

    // SecureRandom
    put("SecureRandom.SHA1PRNG",
	gnu.java.security.provider.SHA1PRNG.class.getName());

  }
}
