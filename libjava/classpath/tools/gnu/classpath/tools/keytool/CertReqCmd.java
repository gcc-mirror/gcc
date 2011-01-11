/* CertReqCmd.java -- The certreq command handler of the keytool
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;
import gnu.java.security.OID;
import gnu.java.security.der.BitString;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;
import gnu.java.util.Base64;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SignatureException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.logging.Logger;

import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.auth.x500.X500Principal;

/**
 * The <b>-certreq</b> keytool command handler is used to generate a Certificate
 * Signing Request (CSR) in PKCS#10 format.
 * <p>
 * The ASN.1 specification of a CSR, as stated in RFC-2986 is as follows:
 * <p>
 * <pre>
 * CertificationRequest ::= SEQUENCE {
 *   certificationRequestInfo  CertificationRequestInfo,
 *   signatureAlgorithm        AlgorithmIdentifier,
 *   signature                 BIT STRING
 * }
 *
 * CertificationRequestInfo ::= SEQUENCE {
 *   version           INTEGER -- v1(0)
 *   subject           Name,
 *   subjectPKInfo     SubjectPublicKeyInfo,
 *   attributes    [0] IMPLICIT Attributes -- see note later
 * }
 *
 * SubjectPublicKeyInfo ::= SEQUENCE {
 *   algorithm         AlgorithmIdentifier,
 *   subjectPublicKey  BIT STRING
 * }
 * </pre>
 * <b>IMPORTANT</b>: Some documentation (e.g. RSA examples) claims that the
 * <code>attributes</code> field is <i>OPTIONAL</i> while <i>RFC-2986</i>
 * implies the opposite. This implementation considers this field, by default,
 * as <i>OPTIONAL</i>, unless the option <code>-attributes</code> is included
 * on the command line.
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
 *      <dt>-sigalg ALGORITHM</dt>
 *      <dd>The canonical name of the digital signature algorithm to use for
 *      signing the certificate. If this option is omitted, a default value will
 *      be chosen based on the type of the private key associated with the
 *      designated <i>Alias</i>. If the private key is a <code>DSA</code> one,
 *      the value for the signature algorithm will be <code>SHA1withDSA</code>.
 *      If on the other hand the private key is an <code>RSA</code> one, then
 *      the tool will use <code>MD5withRSA</code> as the signature algorithm.
 *      <p></dd>
 *
 *      <dt>-file FILE_NAME</dt>
 *
 *      <dt>-keypass PASSWORD</dt>
 *
 *      <dt>-storetype STORE_TYPE</dt>
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
 *      <dd>Use this option to enable more verbose output.
 *      <p></dd>
 *
 *      <dt>-attributes</dt>
 *      <dd>Use this option to force the tool to encode a NULL DER value in the
 *      CSR as the value of the Attributes field.</dd>
 * </dl>
 */
class CertReqCmd extends Command
{
  private static final Logger log = Logger.getLogger(CertReqCmd.class.getName());
  private static final String ATTRIBUTES_OPT = "attributes"; //$NON-NLS-1$
  protected String _alias;
  protected String _sigAlgorithm;
  protected String _certReqFileName;
  protected String _password;
  protected String _ksType;
  protected String _ksURL;
  protected String _ksPassword;
  protected String _providerClassName;
  protected boolean nullAttributes;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param alias the alias to use. */
  public void setAlias(String alias)
  {
    this._alias = alias;
  }

  /**
   * @param algorithm the canonical name of the digital signature algorithm to
   *          use.
   */
  public void setSigalg(String algorithm)
  {
    this._sigAlgorithm = algorithm;
  }

  /** @param pathName the fully qualified path name of the file to process. */
  public void setFile(String pathName)
  {
    this._certReqFileName = pathName;
  }

  /** @param password the (private) key password to use. */
  public void setKeypass(String password)
  {
    this._password = password;
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

  /**
   * @param flag whether to use, or not, a <code>NULL</code> DER value for
   *          the certificate's Attributes field.
   */
  public void setAttributes(String flag)
  {
    this.nullAttributes = Boolean.valueOf(flag).booleanValue();
  }

  // life-cycle methods -------------------------------------------------------

  void setup() throws Exception
  {
    setOutputStreamParam(_certReqFileName);
    setKeyStoreParams(_providerClassName, _ksType, _ksPassword, _ksURL);
    setAliasParam(_alias);
    setKeyPasswordNoPrompt(_password);
    if (Configuration.DEBUG)
      {
        log.fine("-certreq handler will use the following options:"); //$NON-NLS-1$
        log.fine("  -alias=" + alias); //$NON-NLS-1$
        log.fine("  -sigalg=" + _sigAlgorithm); //$NON-NLS-1$
        log.fine("  -file=" + _certReqFileName); //$NON-NLS-1$
        log.fine("  -storetype=" + storeType); //$NON-NLS-1$
        log.fine("  -keystore=" + storeURL); //$NON-NLS-1$
        log.fine("  -provider=" + provider); //$NON-NLS-1$
        log.fine("  -v=" + verbose); //$NON-NLS-1$
        log.fine("  -attributes=" + nullAttributes); //$NON-NLS-1$
      }
  }

  void start() throws KeyStoreException, NoSuchAlgorithmException, IOException,
      UnsupportedCallbackException, UnrecoverableKeyException,
      InvalidKeyException, SignatureException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$
    // 1. get the key entry and certificate chain associated to alias
    Key privateKey = getAliasPrivateKey();
    Certificate[] chain = store.getCertificateChain(alias);

    // 2. get alias's DN and public key to use in the CSR
    X509Certificate bottomCertificate = (X509Certificate) chain[0];
    X500Principal aliasName = bottomCertificate.getIssuerX500Principal();
    PublicKey publicKey = bottomCertificate.getPublicKey();

    // 3. generate the CSR
    setSignatureAlgorithmParam(_sigAlgorithm, privateKey);
    byte[] derBytes = getCSR(aliasName, publicKey, (PrivateKey) privateKey);

    // 4. encode it in base-64 and write it to outStream
    String encoded = Base64.encode(derBytes, 72);
    PrintWriter writer = new PrintWriter(outStream, true);
    writer.println("-----BEGIN NEW CERTIFICATE REQUEST-----"); //$NON-NLS-1$
    writer.println(encoded);
    writer.println("-----END NEW CERTIFICATE REQUEST-----"); //$NON-NLS-1$

    if (verbose)
      {
        if (! systemOut)
          System.out.println(Messages.getFormattedString("CertReqCmd.27", //$NON-NLS-1$
                                                         _certReqFileName));
        System.out.println(Messages.getString("CertReqCmd.28")); //$NON-NLS-1$
      }

    writer.close();
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  Parser getParser()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getParser"); //$NON-NLS-1$
    Parser result = new ClasspathToolParser(Main.CERTREQ_CMD, true);
    result.setHeader(Messages.getString("CertReqCmd.25")); //$NON-NLS-1$
    result.setFooter(Messages.getString("CertReqCmd.24")); //$NON-NLS-1$
    OptionGroup options = new OptionGroup(Messages.getString("CertReqCmd.23")); //$NON-NLS-1$
    options.add(new Option(Main.ALIAS_OPT,
                           Messages.getString("CertReqCmd.22"), //$NON-NLS-1$
                           Messages.getString("CertReqCmd.21")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _alias = argument;
      }
    });
    options.add(new Option(Main.SIGALG_OPT,
                           Messages.getString("CertReqCmd.20"), //$NON-NLS-1$
                           Messages.getString("CertReqCmd.19")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _sigAlgorithm = argument;
      }
    });
    options.add(new Option(Main.FILE_OPT,
                           Messages.getString("CertReqCmd.18"), //$NON-NLS-1$
                           Messages.getString("CertReqCmd.17")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _certReqFileName = argument;
      }
    });
    options.add(new Option(Main.KEYPASS_OPT,
                           Messages.getString("CertReqCmd.16"), //$NON-NLS-1$
                           Messages.getString("CertReqCmd.9")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _password = argument;
      }
    });
    options.add(new Option(Main.STORETYPE_OPT,
                           Messages.getString("CertReqCmd.14"), //$NON-NLS-1$
                           Messages.getString("CertReqCmd.13")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksType = argument;
      }
    });
    options.add(new Option(Main.KEYSTORE_OPT,
                           Messages.getString("CertReqCmd.12"), //$NON-NLS-1$
                           Messages.getString("CertReqCmd.11")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksURL = argument;
      }
    });
    options.add(new Option(Main.STOREPASS_OPT,
                           Messages.getString("CertReqCmd.10"), //$NON-NLS-1$
                           Messages.getString("CertReqCmd.9")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksPassword = argument;
      }
    });
    options.add(new Option(Main.PROVIDER_OPT,
                           Messages.getString("CertReqCmd.8"), //$NON-NLS-1$
                           Messages.getString("CertReqCmd.7")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _providerClassName = argument;
      }
    });
    options.add(new Option(Main.VERBOSE_OPT,
                           Messages.getString("CertReqCmd.6")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        verbose = true;
      }
    });
    options.add(new Option(ATTRIBUTES_OPT,
                           Messages.getString("CertReqCmd.5")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        nullAttributes = true;
      }
    });
    result.add(options);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getParser", result); //$NON-NLS-1$
    return result;
  }

  /**
   * @param aliasName
   * @param publicKey
   * @param privateKey
   * @return the DER encoded Certificate Signing Request.
   * @throws IOException
   * @throws InvalidKeyException
   * @throws SignatureException
   */
  private byte[] getCSR(X500Principal aliasName, PublicKey publicKey,
                        PrivateKey privateKey)
      throws IOException, InvalidKeyException, SignatureException
  {
    DERValue derVersion = new DERValue(DER.INTEGER, BigInteger.ZERO);
    DERValue derSubject = new DERReader(aliasName.getEncoded()).read();
    DERValue derSubjectPKInfo = new DERReader(publicKey.getEncoded()).read();
    byte[] b = nullAttributes ? new byte[] { 0x05, 0x00 } : new byte[0];
    DERValue derAttributes = new DERValue(DER.CONSTRUCTED | DER.CONTEXT | 0,
                                          b.length, b, null);
    ArrayList certRequestInfo = new ArrayList(4);
    certRequestInfo.add(derVersion);
    certRequestInfo.add(derSubject);
    certRequestInfo.add(derSubjectPKInfo);
    certRequestInfo.add(derAttributes);
    DERValue derCertRequestInfo = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                               certRequestInfo);

    OID sigAlgorithmID = getSignatureAlgorithmOID();
    DERValue derSigAlgorithmID = new DERValue(DER.OBJECT_IDENTIFIER,
                                              sigAlgorithmID);
    ArrayList sigAlgorithm = new ArrayList(2);
    sigAlgorithm.add(derSigAlgorithmID);
    if (! sigAlgorithmID.equals(Command.SHA1_WITH_DSA)) // it's an RSA-based
      sigAlgorithm.add(new DERValue(DER.NULL, null));

    sigAlgorithm.trimToSize();
    DERValue derSignatureAlgorithm = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                                  sigAlgorithm);

    signatureAlgorithm.initSign(privateKey);
    signatureAlgorithm.update(derCertRequestInfo.getEncoded());
    byte[] sigBytes = signatureAlgorithm.sign();
    DERValue derSignature = new DERValue(DER.BIT_STRING, new BitString(sigBytes));

    ArrayList csr = new ArrayList(3);
    csr.add(derCertRequestInfo);
    csr.add(derSignatureAlgorithm);
    csr.add(derSignature);
    DERValue derCSR = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, csr);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DERWriter.write(baos, derCSR);
    byte[] result = baos.toByteArray();

    return result;
  }
}
