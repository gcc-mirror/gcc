/* Registry.java -- 
   Copyright (C) 2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

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


package gnu.java.security;

/**
 * A placeholder for <i>names</i> and <i>literals</i> used throughout this
 * library.
 */
public interface Registry
{
  /** The name of our Providers. */
  String GNU_SECURITY = "GNU";
  String GNU_CRYPTO = "GNU-CRYPTO";
  String GNU_SASL = "GNU-SASL";

  /** Our version number. */
  String VERSION_STRING = "2.1.0";

  // Names of properties to use in Maps when initialising primitives .........

  // Symmetric block cipher algorithms and synonyms...........................

  String ANUBIS_CIPHER = "anubis";

  String BLOWFISH_CIPHER = "blowfish";

  String DES_CIPHER = "des";

  String KHAZAD_CIPHER = "khazad";

  String RIJNDAEL_CIPHER = "rijndael";

  String SERPENT_CIPHER = "serpent";

  String SQUARE_CIPHER = "square";

  String TRIPLEDES_CIPHER = "tripledes";

  String TWOFISH_CIPHER = "twofish";

  String CAST5_CIPHER = "cast5";

  String NULL_CIPHER = "null";

  /** AES is synonymous to Rijndael for 128-bit block size only. */
  String AES_CIPHER = "aes";

  /** TripleDES is also known as DESede. */
  String DESEDE_CIPHER = "desede";

  /** CAST5 is also known as CAST-128. */
  String CAST128_CIPHER = "cast128";

  String CAST_128_CIPHER = "cast-128";

  // Key Wrapping Algorithm names and synonyms ...............................

  String KWA_PREFIX = "kw-";
  String AES_KWA = KWA_PREFIX + AES_CIPHER;
  String AES128_KWA = AES_KWA + "128";
  String AES192_KWA = AES_KWA + "192";
  String AES256_KWA = AES_KWA + "256";
  String RIJNDAEL_KWA = KWA_PREFIX + RIJNDAEL_CIPHER;

  String TRIPLEDES_KWA = KWA_PREFIX + TRIPLEDES_CIPHER;
  String DESEDE_KWA = KWA_PREFIX + DESEDE_CIPHER;

  // Message digest algorithms and synonyms...................................

  String WHIRLPOOL_HASH = "whirlpool";

  String RIPEMD128_HASH = "ripemd128";

  String RIPEMD160_HASH = "ripemd160";

  String SHA160_HASH = "sha-160";

  String SHA256_HASH = "sha-256";

  String SHA384_HASH = "sha-384";

  String SHA512_HASH = "sha-512";

  String TIGER_HASH = "tiger";

  String HAVAL_HASH = "haval";

  String MD5_HASH = "md5";

  String MD4_HASH = "md4";

  String MD2_HASH = "md2";

  /** RIPEMD-128 is synonymous to RIPEMD128. */
  String RIPEMD_128_HASH = "ripemd-128";

  /** RIPEMD-160 is synonymous to RIPEMD160. */
  String RIPEMD_160_HASH = "ripemd-160";

  /** SHA-1 is synonymous to SHA-160. */
  String SHA_1_HASH = "sha-1";

  /** SHA1 is synonymous to SHA-160. */
  String SHA1_HASH = "sha1";

  /** SHA is synonymous to SHA-160. */
  String SHA_HASH = "sha";

  // Symmetric block cipher modes of operations...............................

  /** Electronic CodeBook mode. */
  String ECB_MODE = "ecb";

  /** Counter (NIST) mode. */
  String CTR_MODE = "ctr";

  /** Integer Counter Mode (David McGrew). */
  String ICM_MODE = "icm";

  /** Output Feedback Mode (NIST). */
  String OFB_MODE = "ofb";

  /** Cipher block chaining mode (NIST). */
  String CBC_MODE = "cbc";

  /** Cipher feedback mode (NIST). */
  String CFB_MODE = "cfb";

  /** Authenticated-Encrypted mode. */
  String EAX_MODE = "eax";

  // Padding scheme names and synonyms........................................

  /** PKCS#5 padding scheme. */
  String PKCS5_PAD = "pkcs5";

  /** PKCS#7 padding scheme. */
  String PKCS7_PAD = "pkcs7";

  /** Trailing Bit Complement padding scheme. */
  String TBC_PAD = "tbc";

  /** EME-PKCS1-v1_5 padding as described in section 7.2 in RFC-3447. */
  String EME_PKCS1_V1_5_PAD = "eme-pkcs1-v1.5";

  /** SSLv3 padding scheme. */
  String SSL3_PAD = "ssl3";

  /** TLSv1 padding scheme. */
  String TLS1_PAD = "tls1";

  /** ISO 10126-2 padding scheme. */
  String ISO10126_PAD = "iso10126";

  // Pseudo-random number generators..........................................

  /** (Apparently) RC4 keystream PRNG. */
  String ARCFOUR_PRNG = "arcfour";

  /** We use "rc4" as an alias for "arcfour". */
  String RC4_PRNG = "rc4";

  /** PRNG based on David McGrew's Integer Counter Mode. */
  String ICM_PRNG = "icm";

  /** PRNG based on a designated hash function. */
  String MD_PRNG = "md";

  /** PRNG based on UMAC's Key Derivation Function. */
  String UMAC_PRNG = "umac-kdf";

  /**
   * PRNG based on PBKDF2 from PKCS #5 v.2. This is suffixed with the name
   * of a MAC to be used as a PRF.
   */
  String PBKDF2_PRNG_PREFIX = "pbkdf2-";

  /** The continuously-seeded pseudo-random number generator. */
  String CSPRNG_PRNG = "csprng";

  /** The Fortuna PRNG. */
  String FORTUNA_PRNG = "fortuna";

  /** The Fortuna generator PRNG. */
  String FORTUNA_GENERATOR_PRNG = "fortuna-generator";

  // Asymmetric keypair generators............................................

  String DSS_KPG = "dss";

  String RSA_KPG = "rsa";

  String DH_KPG = "dh";

  String SRP_KPG = "srp";

  /** DSA is synonymous to DSS. */
  String DSA_KPG = "dsa";

  // Signature-with-appendix schemes..........................................

  String DSS_SIG = "dss";

  String RSA_SIG_PREFIX = "rsa-";

  String RSA_PSS_ENCODING = "pss";

  String RSA_PSS_SIG = RSA_SIG_PREFIX + RSA_PSS_ENCODING;

  String RSA_PKCS1_V1_5_ENCODING = "pkcs1-v1.5";

  String RSA_PKCS1_V1_5_SIG = RSA_SIG_PREFIX + RSA_PKCS1_V1_5_ENCODING;

  /** DSA is synonymous to DSS. */
  String DSA_SIG = "dsa";

  // Key agreement protocols .................................................

  String DH_KA = "dh";

  String ELGAMAL_KA = "elgamal";

  String SRP6_KA = "srp6";

  String SRP_SASL_KA = "srp-sasl";

  String SRP_TLS_KA = "srp-tls";

  // Keyed-Hash Message Authentication Code ..................................

  /** Name prefix of every HMAC implementation. */
  String HMAC_NAME_PREFIX = "hmac-";

  // Other MAC algorithms ....................................................

  /** The One-key CBC MAC. */
  String OMAC_PREFIX = "omac-";

  /** Message Authentication Code using Universal Hashing (Ted Krovetz). */
  String UHASH32 = "uhash32";

  String UMAC32 = "umac32";

  /** The Truncated Multi-Modular Hash Function -v1 (David McGrew). */
  String TMMH16 = "tmmh16";

  //   String TMMH32 = "tmmh32";

  // Format IDs used to identify how we externalise asymmetric keys ..........
  // fully-qualified names of the supported codecs
  String RAW_ENCODING = "gnu.crypto.raw.format";
  String X509_ENCODING = "gnu.crypto.x509.format";
  String PKCS8_ENCODING = "gnu.crypto.pkcs8.format";
  String ASN1_ENCODING = "gnu.crypto.asn1.format";

  // short names of the same.  used by JCE adapters
  String RAW_ENCODING_SHORT_NAME = "RAW";
  String X509_ENCODING_SORT_NAME = "X.509";
  String PKCS8_ENCODING_SHORT_NAME = "PKCS#8";
  String ASN1_ENCODING_SHORT_NAME = "ASN.1";

  // unique identifiers of the same
  int RAW_ENCODING_ID = 1;
  int X509_ENCODING_ID = 2;
  int PKCS8_ENCODING_ID = 3;
  int ASN1_ENCODING_ID = 4;

  // OID strings used in encoding/decoding keys
  String DSA_OID_STRING = "1.2.840.10040.4.1";
  String RSA_OID_STRING = "1.2.840.113549.1.1.1";
  String DH_OID_STRING =  "1.2.840.10046.2.1";

  // Magic bytes we generate/expect in externalised asymmetric keys ..........
  // the four bytes represent G (0x47) for GNU, 1 (0x01) for Raw format,
  // D (0x44) for DSS, R (0x52) for RSA, H (0x48) for Diffie-Hellman, or S
  // (0x53) for SRP-6, and finally P (0x50) for Public, p (0x70) for private,
  // or S (0x53) for signature.
  byte[] MAGIC_RAW_DSS_PUBLIC_KEY = new byte[] {
      0x47, RAW_ENCODING_ID, 0x44, 0x50 };

  byte[] MAGIC_RAW_DSS_PRIVATE_KEY = new byte[] {
      0x47, RAW_ENCODING_ID, 0x44, 0x70 };

  byte[] MAGIC_RAW_DSS_SIGNATURE = new byte[] {
      0x47, RAW_ENCODING_ID, 0x44, 0x53 };

  byte[] MAGIC_RAW_RSA_PUBLIC_KEY = new byte[] {
      0x47, RAW_ENCODING_ID, 0x52, 0x50 };

  byte[] MAGIC_RAW_RSA_PRIVATE_KEY = new byte[] {
      0x47, RAW_ENCODING_ID, 0x52, 0x70 };

  byte[] MAGIC_RAW_RSA_PSS_SIGNATURE = new byte[] {
      0x47, RAW_ENCODING_ID, 0x52, 0x53 };

  byte[] MAGIC_RAW_RSA_PKCS1V1_5_SIGNATURE = new byte[] {
      0x47, RAW_ENCODING_ID, 0x52, 0x54 };

  byte[] MAGIC_RAW_DH_PUBLIC_KEY = new byte[] {
      0x47, RAW_ENCODING_ID, 0x48, 0x50 };

  byte[] MAGIC_RAW_DH_PRIVATE_KEY = new byte[] {
      0x47, RAW_ENCODING_ID, 0x48, 0x70 };

  byte[] MAGIC_RAW_SRP_PUBLIC_KEY = new byte[] {
      0x47, RAW_ENCODING_ID, 0x53, 0x50 };

  byte[] MAGIC_RAW_SRP_PRIVATE_KEY = new byte[] {
      0x47, RAW_ENCODING_ID, 0x53, 0x70 };

  // SASL Property names .....................................................

  String SASL_PREFIX = "gnu.crypto.sasl";

  /** Name of username property. */
  String SASL_USERNAME = SASL_PREFIX + ".username";

  /** Name of password property. */
  String SASL_PASSWORD = SASL_PREFIX + ".password";

  /** Name of authentication information provider packages. */
  String SASL_AUTH_INFO_PROVIDER_PKGS = SASL_PREFIX + ".auth.info.provider.pkgs";

  /** SASL authorization ID. */
  String SASL_AUTHORISATION_ID = SASL_PREFIX + ".authorisation.ID";

  /** SASL protocol. */
  String SASL_PROTOCOL = SASL_PREFIX + ".protocol";

  /** SASL Server name. */
  String SASL_SERVER_NAME = SASL_PREFIX + ".server.name";

  /** SASL Callback handler. */
  String SASL_CALLBACK_HANDLER = SASL_PREFIX + ".callback.handler";

  /** SASL channel binding. */
  String SASL_CHANNEL_BINDING = SASL_PREFIX + ".channel.binding";

  // SASL data element size limits ...........................................

  /** The size limit, in bytes, of a SASL OS (Octet Sequence) element. */
  int SASL_ONE_BYTE_MAX_LIMIT = 255;

  /**
   * The size limit, in bytes, of both a SASL MPI (Multi-Precision Integer)
   * element and a SASL Text element.
   */
  int SASL_TWO_BYTE_MAX_LIMIT = 65535;

  /** The size limit, in bytes, of a SASL EOS (Extended Octet Sequence) element. */
  int SASL_FOUR_BYTE_MAX_LIMIT = 2147483383;

  /** The size limit, in bytes, of a SASL Buffer. */
  int SASL_BUFFER_MAX_LIMIT = 2147483643;

  // Canonical names of SASL mechanisms ......................................

  String SASL_ANONYMOUS_MECHANISM = "ANONYMOUS";

  String SASL_CRAM_MD5_MECHANISM = "CRAM-MD5";

  String SASL_PLAIN_MECHANISM = "PLAIN";

  String SASL_SRP_MECHANISM = "SRP";

  // Canonical names of Integrity Protection algorithms ......................

  String SASL_HMAC_MD5_IALG = "HMACwithMD5";

  String SASL_HMAC_SHA_IALG = "HMACwithSHA";

  // Quality Of Protection string representations ............................

  /** authentication only. */
  String QOP_AUTH = "auth";

  /** authentication plus integrity protection. */
  String QOP_AUTH_INT = "auth-int";

  /** authentication plus integrity and confidentiality protection. */
  String QOP_AUTH_CONF = "auth-conf";

  // SASL mechanism strength string representation ...........................

  String STRENGTH_HIGH = "high";

  String STRENGTH_MEDIUM = "medium";

  String STRENGTH_LOW = "low";

  // SASL Server Authentication requirement ..................................

  /** Server must authenticate to the client. */
  String SERVER_AUTH_TRUE = "true";

  /** Server does not need to, or cannot, authenticate to the client. */
  String SERVER_AUTH_FALSE = "false";

  // SASL mechanism reuse capability .........................................

  String REUSE_TRUE = "true";

  String REUSE_FALSE = "false";

  // Keyrings  ...............................................................

  byte[] GKR_MAGIC = new byte[] { 0x47, 0x4b, 0x52, 0x01 };

  // Ring usage fields.
  int GKR_PRIVATE_KEYS = 1 << 0;

  int GKR_PUBLIC_CREDENTIALS = 1 << 1;

  int GKR_CERTIFICATES = 1 << 2;

  // HMac types.
  int GKR_HMAC_MD5_128 = 0;

  int GKR_HMAC_SHA_160 = 1;

  int GKR_HMAC_MD5_96 = 2;

  int GKR_HMAC_SHA_96 = 3;

  // Cipher types.
  int GKR_CIPHER_AES_128_OFB = 0;

  int GKR_CIPHER_AES_128_CBC = 1;
}
