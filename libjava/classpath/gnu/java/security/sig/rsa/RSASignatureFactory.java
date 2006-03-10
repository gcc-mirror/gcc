/* RSASignatureFactory.java -- A Factory class to instantiate RSA Signatures
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


package gnu.java.security.sig.rsa;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import gnu.java.security.Registry;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.sig.ISignature;

/**
 * A Factory class to instantiate RSA Signature classes.
 */
public class RSASignatureFactory
{
  private static Set names;

  /**
   * Private constructor to enforce usage through Factory (class) methods.
   */
  private RSASignatureFactory()
  {
    super();
  }

  /**
   * Returns a new instance of an RSA Signature given its name. The name of an
   * RSA Signature always starts with <code>rsa-</code>, followed by either
   * <code>pss</code> or <code>pkcs1_v1.5</code>. An optional message digest
   * name, to be used with the RSA signature may be specified by appending the
   * hyphen chanaracter <code>-</code> followed by the canonical message digest
   * algorithm name. When no message digest algorithm name is given, SHA-160 is
   * used.
   *  
   * @param name the composite RSA signature name.
   * @return a new instance of an RSA Signature algorithm implementation.
   * Returns <code>null</code> if the given name does not correspond to any
   * supported RSA Signature encoding and message digest combination.
   */
  public static final ISignature getInstance(String name)
  {
    if (name == null)
      return null;

    name = name.trim();
    if (name.length() == 0)
      return null;

    name = name.toLowerCase();
    if (! name.startsWith(Registry.RSA_SIG_PREFIX))
      return null;

    name = name.substring(Registry.RSA_SIG_PREFIX.length()).trim();
    if (name.startsWith(Registry.RSA_PSS_ENCODING))
      return getPSSSignature(name);
    else if (name.startsWith(Registry.RSA_PKCS1_V1_5_ENCODING))
      return getPKCS1Signature(name);
    else
      return null;
  }

  /**
   * Returns a {@link Set} of names of <i>RSA</i> signatures supported by this
   * <i>Factory</i>.
   * 
   * @return a {@link Set} of RSA Signature algorithm names (Strings).
   */
  public static synchronized final Set getNames()
  {
    if (names == null)
      {
        Set hashNames = HashFactory.getNames();
        HashSet hs = new HashSet();
        for (Iterator it = hashNames.iterator(); it.hasNext();)
          {
            String mdName = (String) it.next();
            hs.add(Registry.RSA_PSS_SIG + "-" + mdName);
          }

        hs.add(Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.MD2_HASH);
        hs.add(Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.MD5_HASH);
        hs.add(Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA160_HASH);
        hs.add(Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA256_HASH);
        hs.add(Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA384_HASH);
        hs.add(Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA512_HASH);

        names = Collections.unmodifiableSet(hs);
      }

    return names;
  }

  private static final ISignature getPSSSignature(String name)
  {
    name = name.substring(Registry.RSA_PSS_ENCODING.length()).trim();
    // remove the hyphen if found at the beginning
    if (name.startsWith("-"))
      name = name.substring(1).trim();

    IMessageDigest md;
    if (name.length() == 0)
      md = HashFactory.getInstance(Registry.SHA160_HASH);
    else
      {
        // check if there is such a hash
        md = HashFactory.getInstance(name);
        if (md == null)
          return null;
      }

    ISignature result = new RSAPSSSignature(md, 0);
    return result;
  }

  private static final ISignature getPKCS1Signature(String name)
  {
    name = name.substring(Registry.RSA_PKCS1_V1_5_ENCODING.length()).trim();
    // remove the hyphen if found at the beginning
    if (name.startsWith("-"))
      name = name.substring(1).trim();

    IMessageDigest md;
    if (name.length() == 0)
      md = HashFactory.getInstance(Registry.SHA160_HASH);
    else
      {
        // check if there is such a hash
        md = HashFactory.getInstance(name);
        if (md == null)
          return null;
      }

    ISignature result = new RSAPKCS1V1_5Signature(md);
    return result;
  }
}
