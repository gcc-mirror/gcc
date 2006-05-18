/* GenKeyCmd.java -- The genkey command handler of the keytool
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


package gnu.classpath.tools.keytool;

import gnu.java.security.util.Util;
import gnu.java.security.x509.X500DistinguishedName;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyPair;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.util.logging.Logger;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.TextInputCallback;
import javax.security.auth.callback.TextOutputCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/**
 * The <b>-genkey</b> keytool command handler is used to generate a key pair (a
 * public, and associated private keys). It then generates a self-signed X509 v1
 * certificate (authenticating the public key) and stores this certificate and
 * the private key in the key store associating both to a designated alias.
 * <p>
 * Possible options for this command are:
 * <p>
 * <dl>
 *      <dt>-alias ALIAS</dt>
 *      <dd>Every entry, be it a <i>Key Entry</i> or a <i>Trusted
 *      Certificate</i>, in a key store is uniquely identified by a user-defined
 *      <i>Alias</i> string. Use this option to specify the <i>Alias</i> to use
 *      when referring to an entry in the key store. Unless specified otherwise,
 *      a default value of <code>mykey</code> shall be used when this option is
 *      omitted from the command line.
 *      <p></dd>
 *      
 *      <dt>-keyalg ALGORITHM</dt>
 *      <dd>Use this option to specify the canonical name of the key-pair
 *      generation algorithm. The default value for this option is
 *      <code>DSS</code> (a synonym for the Digital Signature Algorithm also
 *      known as <code>DSA</code>).
 *      <p></dd>
 *      
 *      <dt>-keysize KEY_SIZE</dt>
 *      <dd>Use this option to specify the number of bits of the shared modulus
 *      (for both the public and private keys) to use when generating new keys.
 *      A default value of <code>1024</code> will be used if this option is
 *      omitted from the command line.
 *      <p></dd>
 *      
 *      <dt>-sigalg ALGORITHM</dt>
 *      <dd>The canonical name of the digital signature algorithm to use for
 *      signing certificates. If this option is omitted, a default value will be
 *      chosen based on the type of the key-pair; i.e. the algorithm that ends
 *      up being used by the <code>-keyalg</code> option. If the key-pair
 *      generation algorithm is <code>DSA</code>, the value for the signature
 *      algorithm will be <code>SHA1withDSA</code>. If on the other hand the
 *      key-pair generation algorithm is <code>RSA</code>, then the tool will
 *      use <code>MD5withRSA</code> as the signature algorithm.
 *      <p></dd>
 *      
 *      <dt>-dname NAME</dt>
 *      <dd>This a mandatory value for this command. If this option is omitted
 *      the tool will prompt you to enter a <i>Distinguished Name</i> to use as
 *      both the <i>Owner</i> and <i>Issuer</i> of the generated self-signed
 *      certificate.
 *      <p>
 *      The syntax of a valid value for this option MUST follow RFC-2253
 *      specifications. Namely the following components (with their accepted
 *      meaning) will be recognized. Note that the component name is case-
 *      insensitive:
 *      <dl>
 *              <dt>CN</dt>
 *              <dd>The Common Name; e.g. "host.domain.com"</dd>
 *              
 *              <dt>OU</dt>
 *              <dd>The Organizational Unit; e.g. "IT Department"</dd>
 *              
 *              <dt>O</dt>
 *              <dd>The Organization Name; e.g. "The Sample Company"</dd>
 *              
 *              <dt>L</dt>
 *              <dd>The Locality Name; e.g. "Sydney"</dd>
 *              
 *              <dt>ST</dt>
 *              <dd>The State Name; e.g. "New South Wales"</dd>
 *              
 *              <dt>C</dt>
 *              <dd>The 2-letter Country identifier; e.g. "AU"</dd>
 *      </dl>
 *      <p>
 *      When specified with a <code>-dname</code> option, each pair of component
 *      / value will be separated from the other with a comma. Each component
 *      and value pair MUST be separated by an equal sign. For example, the
 *      following is a valid DN value:
 *      <pre>
 *        CN=host.domain.com, O=The Sample Company, L=Sydney, ST=NSW, C=AU
 *      </pre>
 *      If this option is omitted, the tool will prompt you to enter the
 *      information through the console.
 *      <p></dd>
 *      
 *      <dt>-keypass PASSWORD</dt>
 *      <dd>Use this option to specify the password which the tool will use to
 *      protect the newly created Key Entry.
 *      <p>
 *      If this option is omitted, you will be prompted to provide a password.
 *      <p></dd>
 *      
 *      <dt>-validity DAY_COUNT</dt>
 *      
 *      <dt>-storetype STORE_TYP}</dt>
 *      <dd>Use this option to specify the type of the key store to use. The
 *      default value, if this option is omitted, is that of the property
 *      <code>keystore.type</code> in the security properties file, which is
 *      obtained by invoking the {@link java.security.KeyStore#getDefaultType()}
 *      static method.
 *      <p></dd>
 *      
 *      <dt>-keystore URL</dt>
 *      <dd>Use this option to specify the location of the key store to use.
 *      The default value is a file {@link java.net.URL} referencing the file
 *      named <code>.keystore</code> located in the path returned by the call to
 *      {@link java.lang.System#getProperty(String)} using <code>user.home</code>
 *      as argument.
 *      <p>
 *      If a URL was specified, but was found to be malformed --e.g. missing
 *      protocol element-- the tool will attempt to use the URL value as a file-
 *      name (with absolute or relative path-name) of a key store --as if the
 *      protocol was <code>file:</code>.
 *      <p></dd>
 *      
 *      <dt>-storepass PASSWORD</dt>
 *      <dd>Use this option to specify the password protecting the key store. If
 *      this option is omitted from the command line, you will be prompted to
 *      provide a password.
 *      <p></dd>
 *      
 *      <dt>-provider PROVIDER_CLASS_NAME</dt>
 *      <dd>A fully qualified class name of a Security Provider to add to the
 *      current list of Security Providers already installed in the JVM in-use.
 *      If a provider class is specified with this option, and was successfully
 *      added to the runtime --i.e. it was not already installed-- then the tool
 *      will attempt to removed this Security Provider before exiting.
 *      <p></dd>
 *      
 *      <dt>-v</dt>
 *      <dd>Use this option to enable more verbose output.</dd>
 * </dl>
 */
class GenKeyCmd extends Command
{
  private static final Logger log = Logger.getLogger(GenKeyCmd.class.getName());
  /** Default key size in bits. */
  private static final int DEFAULT_KEY_SIZE = 1024;

  private String _alias;
  private String _keyAlgorithm;
  private String _keySizeStr;
  private String _sigAlgorithm;
  private String _dName;
  private String _password;
  private String _validityStr;
  private String _ksType;
  private String _ksURL;
  private String _ksPassword;
  private String _providerClassName;
  private int keySize;
  private X500DistinguishedName distinguishedName;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param alias the alias to use. */
  public void setAlias(String alias)
  {
    this._alias = alias;
  }

  /** @param algorithm the canonical name of the key-pair algorithm to use. */
  public void setKeyalg(String algorithm)
  {
    this._keyAlgorithm = algorithm;
  }

  /**
   * @param bits the string representation of the number of bits (a decimal
   *          positive integer) the modulus of the generated keys (private and
   *          public) should have.
   */
  public void setKeysize(String bits)
  {
    this._validityStr = bits;
  }

  /**
   * @param algorithm the canonical name of the digital signature algorithm to
   *          use.
   */
  public void setSigalg(String algorithm)
  {
    this._sigAlgorithm = algorithm;
  }

  /** @param name the distiniguished name to use. */
  public void setDname(String name)
  {
    this._dName = name;
  }

  /** @param password the (private) key password to use. */
  public void setKeypass(String password)
  {
    this._password = password;
  }

  /**
   * @param days the string representation of the number of days (a decimal,
   *          positive integer) to assign to the generated certificate.
   */
  public void setValidity(String days)
  {
    this._validityStr = days;
  }

  /** @param type the key-store type to use. */
  public void setStoretype(String type)
  {
    this._ksType = type;
  }

  /** @param url the key-store URL to use. */
  public void setKeystore(String url)
  {
    this._ksURL = url;
  }

  /** @param password the key-store password to use. */
  public void setStorepass(String password)
  {
    this._ksPassword = password;
  }

  /** @param className a security provider fully qualified class name to use. */
  public void setProvider(String className)
  {
    this._providerClassName = className;
  }

  // life-cycle methods -------------------------------------------------------

  int processArgs(String[] args, int i)
  {
    int limit = args.length;
    String opt;
    while (++i < limit)
      {
        opt = args[i];
        log.finest("args[" + i + "]=" + opt); //$NON-NLS-1$ //$NON-NLS-2$
        if (opt == null || opt.length() == 0)
          continue;

        if ("-alias".equals(opt)) // -alias ALIAS //$NON-NLS-1$
          _alias = args[++i];
        else if ("-keyalg".equals(opt)) // -keyalg ALGORITHM //$NON-NLS-1$
          _keyAlgorithm = args[++i];
        else if ("-keysize".equals(opt)) // -keysize KEY_SIZE //$NON-NLS-1$
          _keySizeStr = args[++i];
        else if ("-sigalg".equals(opt)) // -sigalg ALGORITHM //$NON-NLS-1$
          _sigAlgorithm = args[++i];
        else if ("-dname".equals(opt)) // -dname NAME //$NON-NLS-1$
          _dName = args[++i];
        else if ("-keypass".equals(opt)) // -keypass PASSWORD //$NON-NLS-1$
          _password = args[++i];
        else if ("-validity".equals(opt)) // -validity DAY_COUNT //$NON-NLS-1$
          _validityStr = args[++i];
        else if ("-storetype".equals(opt)) // -storetype STORE_TYPE //$NON-NLS-1$
          _ksType = args[++i];
        else if ("-keystore".equals(opt)) // -keystore URL //$NON-NLS-1$
          _ksURL = args[++i];
        else if ("-storepass".equals(opt)) // -storepass PASSWORD //$NON-NLS-1$
          _ksPassword = args[++i];
        else if ("-provider".equals(opt)) // -provider PROVIDER_CLASS_NAME //$NON-NLS-1$
          _providerClassName = args[++i];
        else if ("-v".equals(opt)) //$NON-NLS-1$
          verbose = true;
        else
          break;
      }

    return i;
  }

  void setup() throws Exception
  {
    setKeyStoreParams(_providerClassName, _ksType, _ksPassword, _ksURL);
    setAliasParam(_alias);
    setKeyPasswordParam(_password);
    setAlgorithmParams(_keyAlgorithm, _sigAlgorithm);
    setKeySize(_keySizeStr);
    setDName(_dName);
    setValidityParam(_validityStr);

    log.finer("-genkey handler will use the following options:"); //$NON-NLS-1$
    log.finer("  -alias=" + alias); //$NON-NLS-1$
    log.finer("  -keyalg=" + keyPairGenerator.getAlgorithm()); //$NON-NLS-1$
    log.finer("  -keysize=" + keySize); //$NON-NLS-1$
    log.finer("  -sigalg=" + signatureAlgorithm.getAlgorithm()); //$NON-NLS-1$
    log.finer("  -dname=" + distinguishedName); //$NON-NLS-1$
    log.finer("  -keypass=" + String.valueOf(keyPasswordChars)); //$NON-NLS-1$
    log.finer("  -validity=" + validityInDays); //$NON-NLS-1$
    log.finer("  -storetype=" + storeType); //$NON-NLS-1$
    log.finer("  -keystore=" + storeURL); //$NON-NLS-1$
    log.finer("  -storepass=" + String.valueOf(storePasswordChars)); //$NON-NLS-1$
    log.finer("  -provider=" + provider); //$NON-NLS-1$
    log.finer("  -v=" + verbose); //$NON-NLS-1$
  }

  void start() throws CertificateException, KeyStoreException,
      InvalidKeyException, SignatureException, IOException,
      NoSuchAlgorithmException
  {
    log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$

    // 1. generate a new key-pair
    log.fine("About to generate key-pair...");
    keyPairGenerator.initialize(keySize);
    KeyPair kp = keyPairGenerator.generateKeyPair();
    PublicKey publicKey = kp.getPublic();
    PrivateKey privateKey = kp.getPrivate();

    // 2. generate a self-signed certificate
    log.fine("About to generate a self-signed certificate...");
    byte[] derBytes = getSelfSignedCertificate(distinguishedName,
                                               publicKey,
                                               privateKey);
    log.finest(Util.dumpString(derBytes, "derBytes ")); //$NON-NLS-1$
    CertificateFactory x509Factory = CertificateFactory.getInstance(Main.X_509);
    ByteArrayInputStream bais = new ByteArrayInputStream(derBytes);
    Certificate certificate = x509Factory.generateCertificate(bais);
    log.finest("certificate = " + certificate); //$NON-NLS-1$

    // 3. store it, w/ its private key, associating them to alias
    Certificate[] chain = new Certificate[] { certificate };
    log.finest("About to store newly generated material in key store..."); //$NON-NLS-1$
    store.setKeyEntry(alias, privateKey, keyPasswordChars, chain);

    // 4. persist the key store
    saveKeyStore();

    log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  /**
   * @param size the desired key size as a string.
   * @throws NumberFormatException if the string does not represent a valid
   *           decimal integer value.
   */
  private void setKeySize(String size)
  {
    if (size == null || size.trim().length() == 0)
      this.keySize = DEFAULT_KEY_SIZE;
    else
      {
        size = size.trim();
        keySize = Integer.parseInt(size);
        // When generating a DSA key pair, the key size must be in the range
        // from 512 to 1024 bits, and must be a multiple of 64. The default
        // key size for any algorithm is 1024 bits
        if (keySize < 1)
          throw new IllegalArgumentException(Messages.getString("GenKeyCmd.54")); //$NON-NLS-1$
      }
  }

  /**
   * @param name the X.500 distinguished name of the principal for whom the
   *          key/certificate are being generated.
   * @throws UnsupportedCallbackException if no implementation of a name
   *           callback is available.
   * @throws IOException if an I/O related exception occurs during the process.
   * @throws IllegalArgumentException if the designated, or captured, value is
   *           not a valid X.500 distinguished name.
   */
  private void setDName(String name) throws IOException,
      UnsupportedCallbackException
  {
    if (name != null && name.trim().length() > 0)
      name = name.trim();
    else
      {
        // prompt user to provide one
        String dnTxt = Messages.getString("GenKeyCmd.0"); //$NON-NLS-1$
        String oDefault =  Messages.getString("GenKeyCmd.6"); //$NON-NLS-1$
        String lDefault =  Messages.getString("GenKeyCmd.7"); //$NON-NLS-1$
        String stDefault = Messages.getString("GenKeyCmd.8"); //$NON-NLS-1$
        String cDefault =  Messages.getString("GenKeyCmd.9"); //$NON-NLS-1$
        String cnPrompt = Messages.getString("GenKeyCmd.10"); //$NON-NLS-1$
        String oPrompt =  Messages.getFormattedString("GenKeyCmd.11", oDefault); //$NON-NLS-1$
        String ouPrompt = Messages.getString("GenKeyCmd.13"); //$NON-NLS-1$
        String lPrompt =  Messages.getFormattedString("GenKeyCmd.14", lDefault); //$NON-NLS-1$
        String stPrompt = Messages.getFormattedString("GenKeyCmd.16", stDefault); //$NON-NLS-1$
        String cPrompt =  Messages.getFormattedString("GenKeyCmd.18", cDefault); //$NON-NLS-1$

        TextOutputCallback dnCB = new TextOutputCallback(TextOutputCallback.INFORMATION,
                                                         dnTxt);
        TextInputCallback cnCB = new TextInputCallback(cnPrompt);
        TextInputCallback oCB =  new TextInputCallback(oPrompt, oDefault);
        TextInputCallback ouCB = new TextInputCallback(ouPrompt);
        TextInputCallback lCB =  new TextInputCallback(lPrompt, lDefault);
        TextInputCallback sCB =  new TextInputCallback(stPrompt, stDefault);
        TextInputCallback cCB =  new TextInputCallback(cPrompt, cDefault);
        getCallbackHandler().handle(new Callback[] { dnCB, cnCB, oCB, ouCB, lCB, sCB, cCB });
        StringBuilder sb = new StringBuilder();

        // handle CN
        name = parseUserPrompt(cnCB);
        if (name != null && name.length() > 0)
          sb.append("CN=").append(name); //$NON-NLS-1$

        // handle O
        name = parseUserPrompt(oCB);
        if (name != null && name.length() > 0)
          sb.append(",O=").append(name); //$NON-NLS-1$

        // handle OU
        name = parseUserPrompt(ouCB);
        if (name != null && name.length() > 0)
          sb.append(",OU=").append(name.trim()); //$NON-NLS-1$

        // handle L
        name = parseUserPrompt(lCB);
        if (name != null && name.length() > 0)
          sb.append(",L=").append(name.trim()); //$NON-NLS-1$

        // handle ST
        name = parseUserPrompt(sCB);
        if (name != null && name.length() > 0)
          sb.append(",ST=").append(name.trim()); //$NON-NLS-1$

        // handle C
        name = parseUserPrompt(cCB);
        if (name != null && name.length() > 0)
          sb.append(",C=").append(name.trim()); //$NON-NLS-1$

        name = sb.toString().trim();
      }

    log.fine("dName=[" + name + "]"); //$NON-NLS-1$ //$NON-NLS-2$
    distinguishedName = new X500DistinguishedName(name);
  }

  private String parseUserPrompt(TextInputCallback ticb)
  {
    String result = ticb.getText();
    if (result == null || result.trim().length() == 0)
      result = ticb.getDefaultText();
    else if (result.trim().equals(".")) //$NON-NLS-1$
      result = null;
    else
      result = result.trim();

    return result;
  }
}
