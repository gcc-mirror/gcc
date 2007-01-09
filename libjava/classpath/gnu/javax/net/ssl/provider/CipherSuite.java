/* CipherSuite.java -- Supported cipher suites.
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

import gnu.java.security.action.GetSecurityPropertyAction;

import java.io.IOException;
import java.io.OutputStream;

import java.nio.ByteBuffer;

import java.security.AccessController;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.NullCipher;

public final class CipherSuite implements Constructed
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  private static final List<String> tlsSuiteNames = new LinkedList<String>();
  private static final HashMap<String, CipherSuite> namesToSuites = new HashMap<String, CipherSuite>();

  // Core TLS cipher suites.
  public static final CipherSuite TLS_NULL_WITH_NULL_NULL =
    new CipherSuite (CipherAlgorithm.NULL,
                     KeyExchangeAlgorithm.NONE,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.NULL, 0, 0x00, 0x00,
                     "TLS_NULL_WITH_NULL_NULL");
  public static final CipherSuite TLS_RSA_WITH_NULL_MD5 =
    new CipherSuite (CipherAlgorithm.NULL,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.MD5, 0, 0x00, 0x01,
                     "TLS_RSA_WITH_NULL_MD5");
  public static final CipherSuite TLS_RSA_WITH_NULL_SHA =
    new CipherSuite (CipherAlgorithm.NULL,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 0, 0x00, 0x02,
                     "TLS_RSA_WITH_NULL_SHA");
  public static final CipherSuite TLS_RSA_EXPORT_WITH_RC4_40_MD5 =
    new CipherSuite (CipherAlgorithm.RC4,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.MD5, 5, 0x00, 0x03,
                     "TLS_RSA_EXPORT_WITH_RC4_40_MD5");
  public static final CipherSuite TLS_RSA_WITH_RC4_128_MD5 =
    new CipherSuite (CipherAlgorithm.RC4,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.MD5, 16, 0x00, 0x04,
                     "TLS_RSA_WITH_RC4_128_MD5");
  public static final CipherSuite TLS_RSA_WITH_RC4_128_SHA =
    new CipherSuite (CipherAlgorithm.RC4,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x05,
                     "TLS_RSA_WITH_RC4_128_SHA");
  public static final CipherSuite TLS_RSA_EXPORT_WITH_DES40_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 5, 0x00, 0x08,
                     "TLS_RSA_EXPORT_WITH_DES40_CBC_SHA");
  public static final CipherSuite TLS_RSA_WITH_DES_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 8, 0x00, 0x09,
                     "TLS_RSA_WITH_DES_CBC_SHA");
  public static final CipherSuite TLS_RSA_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 24, 0x00, 0x0A,
                     "TLS_RSA_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.DH_DSS,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 5, 0x00, 0x0B,
                     "TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA");
  public static final CipherSuite TLS_DH_DSS_WITH_DES_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.DH_DSS,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 8, 0x00, 0x0C,
                     "TLS_DH_DSS_WITH_DES_CBC_SHA");
  public static final CipherSuite TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.DH_DSS,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 24, 0x00, 0x0D,
                     "TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.DH_RSA,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 5, 0x00, 0x0E,
                     "TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA");
  public static final CipherSuite TLS_DH_RSA_WITH_DES_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.DH_RSA,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 8, 0x00, 0x0F,
                     "TLS_DH_RSA_WITH_DES_CBC_SHA");
  public static final CipherSuite TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.DH_RSA,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 24, 0x00, 0x10,
                     "TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.DHE_DSS, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 5, 0x00, 0x11,
                     "TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA");
  public static final CipherSuite TLS_DHE_DSS_WITH_DES_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.DHE_DSS, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 8, 0x00, 0x12,
                     "TLS_DHE_DSS_WITH_DES_CBC_SHA");
  public static final CipherSuite TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.DHE_DSS, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 24, 0x00, 0x13,
                     "TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.DHE_RSA, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 5, 0x00, 0x14,
                     "TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA");
  public static final CipherSuite TLS_DHE_RSA_WITH_DES_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DES,
                     KeyExchangeAlgorithm.DHE_RSA, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 8, 0x00, 0x15,
                     "TLS_DHE_RSA_WITH_DES_CBC_SHA");
  public static final CipherSuite TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.DHE_RSA, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 24, 0x00, 0x16,
                     "TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA");

  // AES CipherSuites.
  public static final CipherSuite TLS_RSA_WITH_AES_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x2F,
                     "TLS_RSA_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_DH_DSS_WITH_AES_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DH_DSS,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 16, 0x00, 0x30,
                     "TLS_DH_DSS_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_DH_RSA_WITH_AES_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DH_RSA,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 16, 0x00, 0x31,
                     "TLS_DH_RSA_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_DHE_DSS_WITH_AES_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DHE_DSS, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x32,
                     "TLS_DHE_DSS_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_DHE_RSA_WITH_AES_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DHE_RSA, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x33,
                     "TLS_DHE_RSA_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_RSA_WITH_AES_256_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 32, 0x00, 0x35,
                     "TLS_RSA_WITH_AES_256_CBC_SHA");
  public static final CipherSuite TLS_DH_DSS_WITH_AES_256_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DH_DSS,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 32, 0x00, 0x36,
                     "TLS_DH_DSS_WITH_AES_256_CBC_SHA");
  public static final CipherSuite TLS_DH_RSA_WITH_AES_256_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DH_RSA,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 32, 0x00, 0x37,
                     "TLS_DH_RSA_WITH_AES_256_CBC_SHA");
  public static final CipherSuite TLS_DHE_DSS_WITH_AES_256_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DHE_DSS, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 32, 0x00, 0x38,
                     "TLS_DHE_DSS_WITH_AES_256_CBC_SHA");
  public static final CipherSuite TLS_DHE_RSA_WITH_AES_256_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DHE_RSA, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 32, 0x00, 0x39,
                     "TLS_DHE_RSA_WITH_AES_256_CBC_SHA");

  // Secure remote password (SRP) ciphersuites
  // Actual ID values are TBD, so these are omitted until they are specified.
  /*public static final CipherSuite TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 24, 0x00, 0x50,
                     "TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 24, 0x00, 0x51,
                     "TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 24, 0x00, 0x52,
                     "TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_SRP_SHA_WITH_AES_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 16, 0x00, 0x53,
                     "TLS_SRP_SHA_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x54,
                     "TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x55,
                     "TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_SRP_SHA_WITH_AES_256_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.ANONYMOUS,
                     MacAlgorithm.SHA, 32, 0x00, 0x56,
                     "TLS_SRP_SHA_WITH_AES_256_CBC_SHA");
  public static final CipherSuite TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 32, 0x00, 0x57,
                     "TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA");
  public static final CipherSuite TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.SRP,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 32, 0x00, 0x58,
                     "TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA");*/
  
  // Pre-shared key suites.
  public static final CipherSuite TLS_PSK_WITH_RC4_128_SHA =
    new CipherSuite(CipherAlgorithm.RC4,
                    KeyExchangeAlgorithm.PSK,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 16, 0x00, 0x8A,
                    "TLS_PSK_WITH_RC4_128_SHA");
  public static final CipherSuite TLS_PSK_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite(CipherAlgorithm.DESede,
                    KeyExchangeAlgorithm.PSK,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 24, 0x00, 0x8B,
                    "TLS_PSK_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_PSK_WITH_AES_128_CBC_SHA =
    new CipherSuite(CipherAlgorithm.AES,
                    KeyExchangeAlgorithm.PSK,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 16, 0x00, 0x8C,
                    "TLS_PSK_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_PSK_WITH_AES_256_CBC_SHA =
    new CipherSuite(CipherAlgorithm.AES,
                    KeyExchangeAlgorithm.PSK,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 32, 0x00, 0x8D,
                    "TLS_PSK_WITH_AES_256_CBC_SHA");

  public static final CipherSuite TLS_DHE_PSK_WITH_RC4_128_SHA =
    new CipherSuite(CipherAlgorithm.RC4,
                    KeyExchangeAlgorithm.DHE_PSK, true,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 16, 0x00, 0x8E,
                    "TLS_DHE_PSK_WITH_RC4_128_SHA");
  public static final CipherSuite TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite(CipherAlgorithm.DESede,
                    KeyExchangeAlgorithm.DHE_PSK, true,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 24, 0x00, 0x8F,
                    "TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_DHE_PSK_WITH_AES_128_CBC_SHA =
    new CipherSuite(CipherAlgorithm.AES,
                    KeyExchangeAlgorithm.DHE_PSK, true,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 16, 0x00, 0x90,
                    "TLS_DHE_PSK_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_DHE_PSK_WITH_AES_256_CBC_SHA =
    new CipherSuite(CipherAlgorithm.AES,
                    KeyExchangeAlgorithm.DHE_PSK, true,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 32, 0x00, 0x91,
                    "TLS_DHE_PSK_WITH_AES_256_CBC_SHA");
  
  public static final CipherSuite TLS_RSA_PSK_WITH_RC4_128_SHA =
    new CipherSuite(CipherAlgorithm.RC4,
                    KeyExchangeAlgorithm.RSA_PSK,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 16, 0x00, 0x92,
                    "TLS_RSA_PSK_WITH_RC4_128_SHA");
  public static final CipherSuite TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA =
    new CipherSuite(CipherAlgorithm.DESede,
                    KeyExchangeAlgorithm.RSA_PSK,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 24, 0x00, 0x93,
                    "TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA");
  public static final CipherSuite TLS_RSA_PSK_WITH_AES_128_CBC_SHA =
    new CipherSuite(CipherAlgorithm.AES,
                    KeyExchangeAlgorithm.RSA_PSK,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 16, 0x00, 0x94,
                    "TLS_RSA_PSK_WITH_AES_128_CBC_SHA");
  public static final CipherSuite TLS_RSA_PSK_WITH_AES_256_CBC_SHA =
    new CipherSuite(CipherAlgorithm.AES,
                    KeyExchangeAlgorithm.RSA_PSK,
                    SignatureAlgorithm.ANONYMOUS,
                    MacAlgorithm.SHA, 32, 0x00, 0x95,
                    "TLS_RSA_PSK_WITH_AES_256_CBC_SHA");

  // Ciphersuites from the OpenPGP extension draft.
  // These disappeared from a more recent draft.
/*  public static final CipherSuite TLS_DHE_DSS_WITH_CAST_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.CAST5,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x70,
                     "TLS_DHE_DSS_WITH_CAST_128_CBC_SHA");
  public static final CipherSuite TLS_DHE_DSS_WITH_CAST_128_CBC_RMD =
    new CipherSuite (CipherAlgorithm.CAST5,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.HMAC_RMD, 16, 0x00, 0x71,
                     "TLS_DHE_DSS_WITH_CAST_128_CBC_RMD");
  public static final CipherSuite TLS_DHE_DSS_WITH_3DES_EDE_CBC_RMD =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.HMAC_RMD, 24, 0x00, 0x72,
                     "TLS_DHE_DSS_WITH_3DES_EDE_CBC_RMD");
  public static final CipherSuite TLS_DHE_DSS_WITH_AES_128_CBC_RMD =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.HMAC_RMD, 16, 0x00, 0x73,
                     "TLS_DHE_DSS_WITH_AES_128_CBC_RMD");
  public static final CipherSuite TLS_DHE_DSS_WITH_AES_256_CBC_RMD =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.DSA,
                     MacAlgorithm.HMAC_RMD, 32, 0x00, 0x74,
                     "TLS_DHE_DSS_WITH_AES_256_CBC_RMD");
  public static final CipherSuite TLS_DHE_RSA_WITH_CAST_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.CAST5,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x75,
                     "TLS_DHE_RSA_WITH_CAST_128_CBC_SHA");
  public static final CipherSuite TLS_DHE_RSA_WITH_CAST_128_CBC_RMD =
    new CipherSuite (CipherAlgorithm.CAST5,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.HMAC_RMD, 16, 0x00, 0x76,
                     "TLS_DHE_RSA_WITH_CAST_128_CBC_RMD");
  public static final CipherSuite TLS_DHE_RSA_WITH_3DES_EDE_CBC_RMD =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.HMAC_RMD, 24, 0x00, 0x77,
                     "TLS_DHE_RSA_WITH_3DES_EDE_CBC_RMD");
  public static final CipherSuite TLS_DHE_RSA_WITH_AES_128_CBC_RMD =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.HMAC_RMD, 16, 0x00, 0x78,
                     "TLS_DHE_RSA_WITH_AES_128_CBC_RMD");
  public static final CipherSuite TLS_DHE_RSA_WITH_AES_256_CBC_RMD =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.DIFFIE_HELLMAN, true,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.HMAC_RMD, 32, 0x00, 0x79,
                     "TLS_DHE_RSA_WITH_AES_256_CBC_RMD");
  public static final CipherSuite TLS_RSA_WITH_CAST_128_CBC_SHA =
    new CipherSuite (CipherAlgorithm.CAST5,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.SHA, 16, 0x00, 0x7A,
                     "TLS_RSA_WITH_CAST_128_CBC_SHA");
  public static final CipherSuite TLS_RSA_WITH_CAST_128_CBC_RMD =
    new CipherSuite (CipherAlgorithm.CAST5,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.HMAC_RMD, 16, 0x00, 0x7B,
                     "TLS_RSA_WITH_CAST_128_CBC_RMD");
  public static final CipherSuite TLS_RSA_WITH_3DES_EDE_CBC_RMD =
    new CipherSuite (CipherAlgorithm.DESede,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.HMAC_RMD, 24, 0x00, 0x7C,
                     "TLS_RSA_WITH_3DES_EDE_CBC_RMD");
  public static final CipherSuite TLS_RSA_WITH_AES_128_CBC_RMD =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.HMAC_RMD, 16, 0x00, 0x7D,
                     "TLS_RSA_WITH_AES_128_CBC_RMD");
  public static final CipherSuite TLS_RSA_WITH_AES_256_CBC_RMD =
    new CipherSuite (CipherAlgorithm.AES,
                     KeyExchangeAlgorithm.RSA,
                     SignatureAlgorithm.RSA,
                     MacAlgorithm.HMAC_RMD, 32, 0x00, 0x7E,
                     "TLS_RSA_WITH_AES_256_CBC_RMD"); */

  private final CipherAlgorithm cipherAlgorithm;
  private final KeyExchangeAlgorithm keyExchangeAlgorithm;
  private final SignatureAlgorithm signatureAlgorithm;
  private final MacAlgorithm macAlgorithm;
  private final boolean ephemeralDH;
  private final boolean exportable;
  private final boolean isStream;
  private final int keyLength;
  private final byte[] id;
  private final String name;
  private final boolean isResolved;

  // Constructors.
  // -------------------------------------------------------------------------

  private CipherSuite (final CipherAlgorithm cipherAlgorithm,
                       final KeyExchangeAlgorithm keyExchangeAlgorithm,
                       final SignatureAlgorithm signatureAlgorithm,
                       final MacAlgorithm macAlgorithm,
                       final int keyLength,
                       final int id1,
                       final int id2,
                       final String name)
  {
    this (cipherAlgorithm, keyExchangeAlgorithm, false, signatureAlgorithm,
          macAlgorithm, keyLength, id1, id2, name);
  }

  private CipherSuite (final CipherAlgorithm cipherAlgorithm,
                       final KeyExchangeAlgorithm keyExchangeAlgorithm,
                       final boolean ephemeralDH,
                       final SignatureAlgorithm signatureAlgorithm,
                       final MacAlgorithm macAlgorithm,
                       final int keyLength,
                       final int id1,
                       final int id2,
                       final String name)
  {
    this.cipherAlgorithm = cipherAlgorithm;
    this.keyExchangeAlgorithm = keyExchangeAlgorithm;
    this.ephemeralDH = ephemeralDH;
    this.signatureAlgorithm = signatureAlgorithm;
    this.macAlgorithm = macAlgorithm;
    this.exportable = keyLength <= 5;
    this.isStream = (cipherAlgorithm == CipherAlgorithm.NULL
                     || cipherAlgorithm == CipherAlgorithm.RC4);
    this.keyLength = keyLength;
    this.id = new byte[] { (byte) id1, (byte) id2 };
    this.name = name.intern();
    namesToSuites.put(name, this);
    if (name.startsWith("TLS"))
      {
        tlsSuiteNames.add(name);
      }
    isResolved = true;
  }

  private CipherSuite(byte[] id)
  {
    cipherAlgorithm = null;
    keyExchangeAlgorithm = null;
    signatureAlgorithm = null;
    macAlgorithm = null;
    ephemeralDH = false;
    exportable = false;
    isStream = false;
    keyLength = 0;
    this.id = id;
    name = null;
    isResolved = false;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  /**
   * Returns the cipher suite for the given name, or null if there is no
   * such suite.
   *
   * @return The named cipher suite.
   */
  public static CipherSuite forName(String name)
  {
    if (name.startsWith("SSL_"))
      name = "TLS_" + name.substring(4);
    return namesToSuites.get(name);
  }

  public static CipherSuite forValue(final short raw_value)
  {
    byte[] b = new byte[] { (byte) (raw_value >>> 8), (byte) raw_value };
    return new CipherSuite(b).resolve();
  }

  public static List<String> availableSuiteNames()
  {
    return tlsSuiteNames;
  }

  // Intance methods.
  // -------------------------------------------------------------------------

  public CipherAlgorithm cipherAlgorithm ()
  {
    return cipherAlgorithm;
  }

  public Cipher cipher () throws NoSuchAlgorithmException, NoSuchPaddingException
  {
    if (cipherAlgorithm == null)
      throw new NoSuchAlgorithmException (toString () + ": unresolved cipher suite");
    if (cipherAlgorithm == CipherAlgorithm.NULL)
      return new NullCipher ();

    String alg = null;
    if (cipherAlgorithm == CipherAlgorithm.RC4)
      alg = "RC4";
    else
      alg = cipherAlgorithm + "/CBC/NoPadding";
    GetSecurityPropertyAction gspa =
      new GetSecurityPropertyAction ("jessie.jce.provider");
    final String provider = (String) AccessController.doPrivileged (gspa);
    if (provider != null)
      {
        try
          {
            return Cipher.getInstance (alg, provider);
          }
        catch (NoSuchProviderException nspe)
          {
          }
      }
    return Cipher.getInstance (alg);
  }

  public MacAlgorithm macAlgorithm ()
  {
    return macAlgorithm;
  }

  public Mac mac(ProtocolVersion version) throws NoSuchAlgorithmException
  {
    if (macAlgorithm == null)
      throw new NoSuchAlgorithmException(toString() + ": unresolved cipher suite");
    if (macAlgorithm == MacAlgorithm.NULL)
      return null;
    
    String macAlg = null;
    if (version == ProtocolVersion.SSL_3)
      {
        macAlg = "SSLv3HMac-" + macAlgorithm;
      }
    else
      {
        if (macAlgorithm == MacAlgorithm.MD5)
          macAlg = "HMac-MD5";
        if (macAlgorithm == MacAlgorithm.SHA)
          macAlg = "HMac-SHA1";
      }
    
    GetSecurityPropertyAction gspa =
      new GetSecurityPropertyAction ("jessie.jce.provider");
    final String provider = AccessController.doPrivileged (gspa);
    if (provider != null)
      {
        try
          {
            return Mac.getInstance(macAlg, provider);
          }
        catch (NoSuchProviderException nspe)
          {
            // Ignore; try any installed provider.
          }
      }
    return Mac.getInstance(macAlg);
  }

  public SignatureAlgorithm signatureAlgorithm ()
  {
    return signatureAlgorithm;
  }

  public KeyExchangeAlgorithm keyExchangeAlgorithm ()
  {
    return keyExchangeAlgorithm;
  }

  public boolean isEphemeralDH ()
  {
    return ephemeralDH;
  }

  public int length ()
  {
    return 2;
  }

  public void write(OutputStream out) throws IOException
  {
    out.write(id);
  }

  public void put (final ByteBuffer buf)
  {
    buf.put (id);
  }
  
  public CipherSuite resolve()
  {
    if (id[0] == 0x00) switch (id[1] & 0xFF)
      {
      case 0x00: return TLS_NULL_WITH_NULL_NULL;
      case 0x01: return TLS_RSA_WITH_NULL_MD5;
      case 0x02: return TLS_RSA_WITH_NULL_SHA;
      case 0x03: return TLS_RSA_EXPORT_WITH_RC4_40_MD5;
      case 0x04: return TLS_RSA_WITH_RC4_128_MD5;
      case 0x05: return TLS_RSA_WITH_RC4_128_SHA;
      case 0x08: return TLS_RSA_EXPORT_WITH_DES40_CBC_SHA;
      case 0x09: return TLS_RSA_WITH_DES_CBC_SHA;
      case 0x0A: return TLS_RSA_WITH_3DES_EDE_CBC_SHA;
      case 0x0B: return TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA;
      case 0x0C: return TLS_DH_DSS_WITH_DES_CBC_SHA;
      case 0x0D: return TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA;
      case 0x0E: return TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA;
      case 0x0F: return TLS_DH_RSA_WITH_DES_CBC_SHA;
      case 0x10: return TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA;
      case 0x11: return TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA;
      case 0x12: return TLS_DHE_DSS_WITH_DES_CBC_SHA;
      case 0x13: return TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
      case 0x14: return TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA;
      case 0x15: return TLS_DHE_RSA_WITH_DES_CBC_SHA;
      case 0x16: return TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA;
      case 0x2F: return TLS_RSA_WITH_AES_128_CBC_SHA;
      case 0x30: return TLS_DH_DSS_WITH_AES_128_CBC_SHA;
      case 0x31: return TLS_DH_RSA_WITH_AES_128_CBC_SHA;
      case 0x32: return TLS_DHE_DSS_WITH_AES_128_CBC_SHA;
      case 0x33: return TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
      case 0x35: return TLS_RSA_WITH_AES_256_CBC_SHA;
      case 0x36: return TLS_DH_DSS_WITH_AES_256_CBC_SHA;
      case 0x37: return TLS_DH_RSA_WITH_AES_256_CBC_SHA;
      case 0x38: return TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
      case 0x39: return TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
      /*case 0x50: return TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA;
      case 0x51: return TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA;
      case 0x52: return TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA;
      case 0x53: return TLS_SRP_SHA_WITH_AES_128_CBC_SHA;
      case 0x54: return TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA;
      case 0x55: return TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA;
      case 0x56: return TLS_SRP_SHA_WITH_AES_256_CBC_SHA;
      case 0x57: return TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA;
      case 0x58: return TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA;
      case 0x70: return TLS_DHE_DSS_WITH_CAST_128_CBC_SHA;
      case 0x71: return TLS_DHE_DSS_WITH_CAST_128_CBC_RMD;
      case 0x72: return TLS_DHE_DSS_WITH_3DES_EDE_CBC_RMD;
      case 0x73: return TLS_DHE_DSS_WITH_AES_128_CBC_RMD;
      case 0x74: return TLS_DHE_DSS_WITH_AES_256_CBC_RMD;
      case 0x75: return TLS_DHE_RSA_WITH_CAST_128_CBC_SHA;
      case 0x76: return TLS_DHE_RSA_WITH_CAST_128_CBC_RMD;
      case 0x77: return TLS_DHE_RSA_WITH_3DES_EDE_CBC_RMD;
      case 0x78: return TLS_DHE_RSA_WITH_AES_128_CBC_RMD;
      case 0x79: return TLS_DHE_RSA_WITH_AES_256_CBC_RMD;
      case 0x7A: return TLS_RSA_WITH_CAST_128_CBC_SHA;
      case 0x7B: return TLS_RSA_WITH_CAST_128_CBC_RMD;
      case 0x7C: return TLS_RSA_WITH_3DES_EDE_CBC_RMD;
      case 0x7D: return TLS_RSA_WITH_AES_128_CBC_RMD;
      case 0x7E: return TLS_RSA_WITH_AES_256_CBC_RMD;*/
      case 0x8A: return TLS_PSK_WITH_RC4_128_SHA;
      case 0x8B: return TLS_PSK_WITH_3DES_EDE_CBC_SHA;
      case 0x8C: return TLS_PSK_WITH_AES_128_CBC_SHA;
      case 0x8D: return TLS_PSK_WITH_AES_256_CBC_SHA;
      case 0x8E: return TLS_DHE_PSK_WITH_RC4_128_SHA;
      case 0x8F: return TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA;
      case 0x90: return TLS_DHE_PSK_WITH_AES_128_CBC_SHA;
      case 0x91: return TLS_DHE_PSK_WITH_AES_256_CBC_SHA;
      case 0x92: return TLS_RSA_PSK_WITH_RC4_128_SHA;
      case 0x93: return TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA;
      case 0x94: return TLS_RSA_PSK_WITH_AES_128_CBC_SHA;
      case 0x95: return TLS_RSA_PSK_WITH_AES_256_CBC_SHA;
      }
    return this;
  }
  
  public boolean isResolved()
  {
    return isResolved;
  }

  public int keyLength()
  {
    return keyLength;
  }

  public boolean isExportable()
  {
    return exportable;
  }

  public boolean isStreamCipher()
  {
    return isStream;
  }

//   String getAuthType()
//   {
//     if (keyExchangeAlgorithm == KeyExchangeAlgorithm.RSA)
//       {
//         if (isExportable())
//           {
//             return "RSA_EXPORT";
//           }
//         return "RSA";
//       }
//     return kexName + "_" + sigName;
//   }

  public byte[] id()
  {
    return id;
  }

  public boolean equals(Object o)
  {
    if (!(o instanceof CipherSuite))
      {
        return false;
      }
    if (o == this)
      return true;
    byte[] id = ((CipherSuite) o).id();
    return (id[0] == this.id[0] &&
            id[1] == this.id[1]);
  }

  public int hashCode()
  {
    return 0xFFFF0000 | (id[0] & 0xFF) << 8 | (id[1] & 0xFF);
  }

  public String toString (String prefix)
  {
    return toString ();
  }

  public String toString()
  {
    if (name == null)
      {
        return "{ " + (id[0] & 0xFF) + ", " + (id[1] & 0xFF) + " }";
      }
    return name;
  }
}
