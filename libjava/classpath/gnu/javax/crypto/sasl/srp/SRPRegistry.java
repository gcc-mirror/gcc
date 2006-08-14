/* SRPRegistry.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.sasl.srp;

import gnu.java.security.Registry;

/**
 * A list of key names designating the values exchanged between the server
 * and client in an SRP communication authentication phase.
 */
public interface SRPRegistry
{
  /** Indices of (N, g) parameter values for SRP (.conf) password database. */
  String N_2048_BITS = "1";
  String N_1536_BITS = "2";
  String N_1280_BITS = "3";
  String N_1024_BITS = "4";
  String N_768_BITS = "5";
  String N_640_BITS = "6";
  String N_512_BITS = "7";
  /** Available hash algorithms for all SRP calculations. */
  String[] SRP_ALGORITHMS = {
      Registry.SHA160_HASH, // the default one
      Registry.MD5_HASH,
      Registry.RIPEMD128_HASH,
      Registry.RIPEMD160_HASH,

      Registry.SHA256_HASH,
      Registry.SHA384_HASH,
      Registry.SHA512_HASH };
  /**
   * The name of the default message digest algorithm to use when no name is
   * explicitely given. In this implementation it is the <b>first</b> among
   * those supported; i.e. the algorithm at index position #0: SHA with
   * 160-bit output.
   */
  String SRP_DEFAULT_DIGEST_NAME = SRP_ALGORITHMS[0];
  /**
   * The property name of the message digest algorithm name to use in a given
   * SRP incarnation.
   */
  String SRP_DIGEST_NAME = "srp.digest.name";
  /** The public shared modulus: n. */
  String SHARED_MODULUS = "srp.N";
  /** The GF generator used: g. */
  String FIELD_GENERATOR = "srp.g";
  /** The list of server's available security options. */
  String AVAILABLE_OPTIONS = "srp.L";
  /** The client's chosen security options. */
  String CHOSEN_OPTIONS = "srp.o";
  /** The client's username. */
  String USER_NAME = "srp.U";
  /** The client's authorization ID. */
  String USER_ROLE = "srp.I";
  /** The user's salt. */
  String USER_SALT = "srp.s";
  /** The user's password verifier. */
  String PASSWORD_VERIFIER = "srp.v";
  /** The client's public ephemeral exponent: A. */
  String CLIENT_PUBLIC_KEY = "srp.A";
  /** The server's public ephemeral exponent: B. */
  String SERVER_PUBLIC_KEY = "srp.B";
  /** The client's evidence: M1. */
  String CLIENT_EVIDENCE = "srp.M1";
  /** The server's evidence: M2. */
  String SERVER_EVIDENCE = "srp.M2";
  /** Name of underlying hash algorithm for use with all SRP calculations. */
  String SRP_HASH = "gnu.crypto.sasl.srp.hash";
  /** Name of SRP mandatory service property. */
  String SRP_MANDATORY = "gnu.crypto.sasl.srp.mandatory";
  /** Name of SRP replay detection property. */
  String SRP_REPLAY_DETECTION = "gnu.crypto.sasl.srp.replay.detection";
  /** Name of SRP integrity protection property. */
  String SRP_INTEGRITY_PROTECTION = "gnu.crypto.sasl.srp.integrity";
  /** Name of SRP confidentiality protection property. */
  String SRP_CONFIDENTIALITY = "gnu.crypto.sasl.srp.confidentiality";
  /** Name of the main SRP password file pathname property. */
  String PASSWORD_FILE = "gnu.crypto.sasl.srp.password.file";
  /**
   * Name of the SRP password database property --a reference to
   * {@link PasswordFile} object.
   */
  String PASSWORD_DB = "gnu.crypto.sasl.srp.password.db";
  /** Default fully qualified pathname of the SRP password file. */
  String DEFAULT_PASSWORD_FILE = "/etc/tpasswd";
  /** Default value for replay detection security service. */
  boolean DEFAULT_REPLAY_DETECTION = true;
  /** Default value for integrity protection security service. */
  boolean DEFAULT_INTEGRITY = true; // implied by the previous option
  /** Default value for confidentiality protection security service. */
  boolean DEFAULT_CONFIDENTIALITY = false;
  // constants defining HMAC names
  String HMAC_SHA1 = "hmac-sha1";
  String HMAC_MD5 = "hmac-md5";
  String HMAC_RIPEMD_160 = "hmac-ripemd-160";
  /** Available HMAC algorithms for integrity protection. */
  String[] INTEGRITY_ALGORITHMS = { HMAC_SHA1, HMAC_MD5, HMAC_RIPEMD_160 };
  // constants defining Cipher names
  String AES = "aes";
  String BLOWFISH = "blowfish";
  /** Available Cipher algorithms for confidentiality protection. */
  String[] CONFIDENTIALITY_ALGORITHMS = { AES, BLOWFISH };
  /** String for mandatory replay detection. */
  String OPTION_MANDATORY = "mandatory";
  /** String for mda: the SRP digest algorithm name. */
  String OPTION_SRP_DIGEST = "mda";
  /** String for mandatory replay detection. */
  String OPTION_REPLAY_DETECTION = "replay_detection";
  /** String for mandatory integrity protection. */
  String OPTION_INTEGRITY = "integrity";
  /** String for mandatory confidentiality protection. */
  String OPTION_CONFIDENTIALITY = "confidentiality";
  /** String for mandatory replay detection. */
  String OPTION_MAX_BUFFER_SIZE = "maxbuffersize";
  /** String for no mandatory security service. */
  String MANDATORY_NONE = "none";
  /** Default mandatory security service required. */
  String DEFAULT_MANDATORY = OPTION_REPLAY_DETECTION;
  /** Name of the UID field in the plain password file. */
  String MD_NAME_FIELD = "srp.md.name";
  /** Name of the GID field in the plain password file. */
  String USER_VERIFIER_FIELD = "srp.user.verifier";
  /** Name of the GECOS field in the plain password file. */
  String SALT_FIELD = "srp.salt";
  /** Name of the SHELL field in the plain password file. */
  String CONFIG_NDX_FIELD = "srp.config.ndx";
  /** Minimum bitlength of the SRP public modulus. */
  int MINIMUM_MODULUS_BITLENGTH = 512;
}
