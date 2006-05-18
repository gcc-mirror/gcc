/* ImportCmd.java -- The import command handler of the keytool
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

import gnu.classpath.SystemProperties;
import gnu.java.security.x509.X509CertPath;

import java.io.FileInputStream;
import java.io.IOException;
import java.security.Key;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertPathValidator;
import java.security.cert.CertPathValidatorException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.PKIXCertPathValidatorResult;
import java.security.cert.PKIXParameters;
import java.security.interfaces.DSAParams;
import java.security.interfaces.DSAPublicKey;
import java.security.interfaces.RSAPublicKey;
import java.util.Collection;
import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.ConfirmationCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/**
 * The <code>-import</code> keytool command handler is used to read an X.509
 * certificate, or a PKCS#7 Certificate Reply from a designated input source and
 * incorporate the certificates into the key store.
 * <p>
 * If the <i>Alias</i> does not already exist in the key store, the tool treats
 * the certificate read from the input source as a new Trusted Certificate. It
 * then attempts to discover a chain-of-trust, starting from that certificate
 * and ending at another <i>Trusted Certificate</i>, already stored in the key
 * store.  If the <code>-trustcacerts</code> option is present, an additional
 * key store, of type <code>JKS</code> named <code>cacerts</code>, and assumed
 * to be present in <code>${JAVA_HOME}/lib/security</code> will also be
 * consulted if found --<code>${JAVA_HOME}</code> refers to the location of an
 * installed Java Runtime Environment (JRE). If no chain-of-trust can be
 * established, and unless the <code>-noprompt</code> option has been specified,
 * the certificate is printed to STDOUT and the user is prompted for a
 * confirmation.
 * <p>
 * If <i>Alias</i> exists in the key store, the tool will treat the
 * certificate(s) read from the input source as a <i>Certificate Reply</i>,
 * which can be a chain of certificates, that eventually would replace the chain
 * of certificates associated with the <i>Key Entry</i> of that <i>Alias</i>.
 * The substitution of the certificates only occurs if a chain-of-trust can be
 * established between the bottom certificate of the chain read from the input
 * file and the <i>Trusted Certificates</i> already present in the key store.
 * Again, if the <code>-trustcacerts</code> option is specified, additional
 * <i>Trusted Certificates</i> in the same <code>cacerts</code> key store will
 * be considered. If no chain-of-trust can be established, the operation will
 * abort.
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
 *      <dt>-file FILE_NAME</dt>
 *      <dd>The fully qualified path of the file to read from. If omitted, the
 *      tool will process STDIN.
 *      <p></dd>
 *      
 *      <dt>-keypass PASSWORD</dt>
 *      <dd>Use this option to specify the password which the tool will use to
 *      protect the <i>Key Entry</i> associated with the designated <i>Alias</i>,
 *      when replacing this <i>Alias</i>' chain of certificates with that found
 *      in the certificate reply.
 *      <p>
 *      If this option is omitted, and the chain-of-trust for the certificate
 *      reply has been established, the tool will first attempt to unlock the
 *      <i>Key Entry</i> using the same password protecting the key store. If
 *      this fails, you will then be prompted to provide a password.
 *      <p></dd>
 *      
 *      <dt>-noprompt</dt>
 *      <dd>Use this option to prevent the tool from prompting the user.
 *      <p></dd>
 *      
 *      <dt>-trustcacerts</dt>
 *      <dd>Use this option to indicate to the tool that a key store, of type
 *      <code>JKS</code>, named <code>cacerts</code>, and usually located in
 *      <code>lib/security</code> in an installed Java Runtime Environment
 *      should be considered when trying to establish chain-of-trusts.
 *      <p></dd>
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
class ImportCmd extends Command
{
  private static final Logger log = Logger.getLogger(ImportCmd.class.getName());
  private String _alias;
  private String _certFileName;
  private String _password;
  private boolean noPrompt;
  private boolean trustCACerts;
  private String _ksType;
  private String _ksURL;
  private String _ksPassword;
  private String _providerClassName;
  private CertificateFactory x509Factory;
  private boolean imported;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param alias the existing alias to use. */
  public void setAlias(String alias)
  {
    this._alias = alias;
  }

  /** @param pathName the fully qualified path name of the file to process. */
  public void setFile(String pathName)
  {
    this._certFileName = pathName;
  }

  /** @param password the existing (private) key password to use. */
  public void setKeypass(String password)
  {
    this._password = password;
  }

  /**
   * @param flag whether to prompt, or not, the user to verify certificate
   *          fingerprints.
   */
  public void setNoprompt(String flag)
  {
    this.noPrompt = Boolean.valueOf(flag).booleanValue();
  }

  /**
   * @param flag whether to trust, or not, certificates found in the
   *          <code>cacerts</code> key store.
   */
  public void setTrustcacerts(String flag)
  {
    this.trustCACerts = Boolean.valueOf(flag).booleanValue();
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
        else if ("-file".equals(opt)) // -file FILE_NAME //$NON-NLS-1$
          _certFileName = args[++i];
        else if ("-keypass".equals(opt)) // -keypass PASSWORD //$NON-NLS-1$
          _password = args[++i];
        else if ("-noprompt".equals(opt)) //$NON-NLS-1$
          noPrompt = true;
        else if ("-trustcacerts".equals(opt)) //$NON-NLS-1$
          trustCACerts = true;
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
    setInputStreamParam(_certFileName);
    setKeyStoreParams(_providerClassName, _ksType, _ksPassword, _ksURL);
    setAliasParam(_alias);
    setKeyPasswordNoPrompt(_password);

    log.finer("-import handler will use the following options:"); //$NON-NLS-1$
    log.finer("  -alias=" + alias); //$NON-NLS-1$
    log.finer("  -file=" + _certFileName); //$NON-NLS-1$
    log.finer("  -keypass=" + _password); //$NON-NLS-1$
    log.finer("  -noprompt=" + noPrompt); //$NON-NLS-1$
    log.finer("  -trustcacerts=" + trustCACerts); //$NON-NLS-1$
    log.finer("  -storetype=" + storeType); //$NON-NLS-1$
    log.finer("  -keystore=" + storeURL); //$NON-NLS-1$
    log.finer("  -storepass=" + String.valueOf(storePasswordChars)); //$NON-NLS-1$
    log.finer("  -provider=" + provider); //$NON-NLS-1$
    log.finer("  -v=" + verbose); //$NON-NLS-1$
  }

  void start() throws CertificateException, KeyStoreException, IOException,
      UnsupportedCallbackException, NoSuchAlgorithmException,
      CertPathValidatorException, UnrecoverableKeyException
  {
    log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$

    x509Factory = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
    // the alias will tell us whether we're dealing with
    // a new trusted certificate or a certificate reply
    if (! store.containsAlias(alias))
      importNewTrustedCertificate();
    else
      {
        ensureAliasIsKeyEntry();
        importCertificateReply();
      }

    log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  /**
   * When importing a new trusted certificate, <i>alias</i> MUST NOT yet exist
   * in the key store.
   * <p>
   * Before adding the certificate to the key store and associate it with the
   * designated Alias, this method tries to verify it by attempting to construct
   * a chain of trust from that certificate to a self-signed certificate
   * (belonging to a root CA), using (already) trusted certificates that are
   * available in the key store.
   * <p>
   * If the <code>-trustcacerts</code> option was detected on the command
   * line, additional trusted certificates are considered for establishing the
   * chain of trust. Those additional certificates are assumed to be in a key
   * store, of type <code>JKS</code> named <code>cacerts</code> and usually
   * located in <code>${JAVA_HOME}/lib/security</code>, where
   * <code>${JAVA_HOME}</code> is the root folder location of a Java runtime.
   * <p>
   * If this method fails to establish a trust path from the certificate to be
   * imported up to a trusted self-signed certificate, the certificate is
   * printed to <code>STDOUT</code>, and the user is prompted to verify it,
   * with the option of aborting the import operation. If however the option
   * <code>-noprompt</code> was detected on the command line, no interaction
   * with the user will take place and the import operation will abort.
   * 
   * @throws CertificateException
   * @throws KeyStoreException
   * @throws NoSuchAlgorithmException
   * @throws UnsupportedCallbackException 
   * @throws IOException 
   * @throws UnrecoverableKeyException 
   * @throws CertPathValidatorException 
   */
  private void importNewTrustedCertificate() throws CertificateException,
      KeyStoreException, NoSuchAlgorithmException, IOException,
      UnsupportedCallbackException, CertPathValidatorException,
      UnrecoverableKeyException
  {
    log.entering(this.getClass().getName(), "importNewTrustedCertificate"); //$NON-NLS-1$

    Certificate certificate = x509Factory.generateCertificate(inStream);
    log.finest("certificate = " + certificate); //$NON-NLS-1$
    LinkedList orderedReply = new LinkedList();
    orderedReply.addLast(certificate);

    if (findTrustAndUpdate(orderedReply, ! noPrompt))
      {
        store.setCertificateEntry(alias, certificate);
        System.out.println(Messages.getString("ImportCmd.29")); //$NON-NLS-1$
        saveKeyStore();
      }
    else
      System.out.println(Messages.getString("ImportCmd.28")); //$NON-NLS-1$

    log.exiting(this.getClass().getName(), "importNewTrustedCertificate"); //$NON-NLS-1$
  }

  /**
   * A certificate reply is a certificate, whose Owner is stored in the key
   * store associated to the designated Alias, and now signed by supposedly a
   * trusted CA (Certificate Authority). In other words, the Subject in this
   * certificate reply is Alias's own and the Issuer is a CA.
   * <p>
   * When importing a certificate reply, the reply is validated using trusted
   * certificates from the key store, and optionally (if the option
   * <code>-trustcacerts</code> was detected on the command line) certificates
   * found in the key store, of type <code>JKS</code> named <code>cacerts</code>
   * located in <code>${JAVA_HOME}/lib/security</code>, where
   * <code>${JAVA_HOME}</code> is the root folder location of a Java runtime.
   * 
   * @throws CertificateException
   * @throws UnsupportedCallbackException
   * @throws IOException
   * @throws KeyStoreException
   * @throws CertPathValidatorException
   * @throws NoSuchAlgorithmException
   * @throws UnrecoverableKeyException
   */
  private void importCertificateReply() throws CertificateException,
      IOException, UnsupportedCallbackException, KeyStoreException,
      NoSuchAlgorithmException, CertPathValidatorException,
      UnrecoverableKeyException
  {
    log.entering(this.getClass().getName(), "importCertificateReply"); //$NON-NLS-1$

    Collection certificates = x509Factory.generateCertificates(inStream);
    ensureReplyIsOurs(certificates);
    // we now have established that the public keys are the same.
    // find a chain-of-trust if one exists
    if (certificates.size() == 1)
      importCertificate((Certificate) certificates.iterator().next());
    else
      importChain(certificates);

    log.exiting(this.getClass().getName(), "importCertificateReply"); //$NON-NLS-1$
  }

  /**
   * If the reply is a single X.509 certificate, keytool attempts to establish a
   * trust chain, starting at the certificate reply and ending at a self-signed
   * certificate (belonging to a root CA). The certificate reply and the
   * hierarchy of certificates used to authenticate the certificate reply form
   * the new certificate chain of alias. If a trust chain cannot be established,
   * the certificate reply is not imported. In this case, keytool does not print
   * out the certificate, nor does it prompt the user to verify it. This is
   * because it is very hard (if not impossible) for a user to determine the
   * authenticity of the certificate reply.
   * 
   * @param certificate the certificate reply to import into the key store.
   * @throws NoSuchAlgorithmException 
   * @throws CertPathValidatorException 
   * @throws UnsupportedCallbackException 
   * @throws IOException 
   * @throws UnrecoverableKeyException 
   * @throws KeyStoreException 
   * @throws CertificateException 
   */
  private void importCertificate(Certificate certificate)
      throws NoSuchAlgorithmException, CertPathValidatorException,
      KeyStoreException, UnrecoverableKeyException, IOException,
      UnsupportedCallbackException, CertificateException
  {
    log.entering(this.getClass().getName(), "importCertificate", certificate); //$NON-NLS-1$

    LinkedList reply = new LinkedList();
    reply.addLast(certificate);

    if (! findTrustAndUpdate(reply, false))
      throw new CertPathValidatorException(Messages.getString("ImportCmd.34")); //$NON-NLS-1$

    Certificate[] newChain = (Certificate[]) reply.toArray(new Certificate[0]);
    Key privateKey = getAliasPrivateKey();
    store.setKeyEntry(alias, privateKey, keyPasswordChars, newChain);
    saveKeyStore();

    log.exiting(this.getClass().getName(), "importCertificate"); //$NON-NLS-1$
  }

  /**
   * If the reply is a PKCS#7 formatted certificate chain, the chain is first
   * ordered (with the user certificate first and the self-signed root CA
   * certificate last), before keytool attempts to match the root CA certificate
   * provided in the reply with any of the trusted certificates in the key store
   * or the "cacerts" keystore file (if the -trustcacerts option was specified).
   * If no match can be found, the information of the root CA certificate is
   * printed out, and the user is prompted to verify it, e.g., by comparing the
   * displayed certificate fingerprints with the fingerprints obtained from some
   * other (trusted) source of information, which might be the root CA itself.
   * The user then has the option of aborting the import operation. If the
   * -noprompt option is given, however, there will be no interaction with the
   * user.
   * 
   * @param chain the collection of certificates parsed from the user
   *          designated input.
   * @throws UnsupportedCallbackException 
   * @throws IOException 
   * @throws UnrecoverableKeyException 
   * @throws KeyStoreException 
   * @throws CertPathValidatorException 
   * @throws NoSuchAlgorithmException 
   * @throws CertificateException 
   */
  private void importChain(Collection chain) throws NoSuchAlgorithmException,
      CertPathValidatorException, KeyStoreException, UnrecoverableKeyException,
      IOException, UnsupportedCallbackException, CertificateException
  {
    log.entering(this.getClass().getName(), "importChain", chain); //$NON-NLS-1$

    LinkedList reply = orderChain(chain);
    if (findTrustAndUpdate(reply, ! noPrompt))
      {
        Certificate[] newChain = (Certificate[]) reply.toArray(new Certificate[0]);
        Key privateKey = getAliasPrivateKey();
        store.setKeyEntry(alias, privateKey, keyPasswordChars, newChain);
        saveKeyStore();
      }

    log.exiting(this.getClass().getName(), "importChain"); //$NON-NLS-1$
  }

  /**
   * Check to ensure that alias's public key is the subject of the first
   * certificate in the passed certificate collection. Throws an exception if
   * the public keys do not match.
   * 
   * @param certificates a {@link Collection} of certificate replies (either a
   *          signle certificate reply, or a PKCS#7 certificate reply chain)
   *          usually sent by a CA as a response to a Certificate Signing
   *          Request (CSR).
   * @throws IOException
   * @throws UnsupportedCallbackException
   * @throws KeyStoreException
   */
  private void ensureReplyIsOurs(Collection certificates) throws IOException,
      UnsupportedCallbackException, KeyStoreException
  {
    log.entering(this.getClass().getName(), "ensureReplyIsOurs"); //$NON-NLS-1$

    Certificate certificate = (Certificate) certificates.iterator().next();
    log.finest("certificate = " + certificate); //$NON-NLS-1$
    Certificate[] chain = store.getCertificateChain(alias);
    if (chain == null)
      throw new IllegalArgumentException(Messages.getFormattedString("ImportCmd.37", //$NON-NLS-1$
                                                                     alias));
    Certificate anchor = chain[0];
    PublicKey anchorPublicKey = anchor.getPublicKey();
    PublicKey certPublicKey = certificate.getPublicKey();
    boolean sameKey;
    if (anchorPublicKey instanceof DSAPublicKey)
      {
        DSAPublicKey pk1 = (DSAPublicKey) anchorPublicKey;
        if (!(certPublicKey instanceof DSAPublicKey))
          throw new IllegalArgumentException(Messages.getString("ImportCmd.38")); //$NON-NLS-1$

        sameKey = areEqual(pk1, (DSAPublicKey) certPublicKey);
      }
    else if (anchorPublicKey instanceof RSAPublicKey)
      {
        RSAPublicKey pk1 = (RSAPublicKey) anchorPublicKey;
        if (!(certPublicKey instanceof RSAPublicKey))
          throw new IllegalArgumentException(Messages.getString("ImportCmd.38")); //$NON-NLS-1$

        sameKey = areEqual(pk1, (RSAPublicKey) certPublicKey);
      }
    else
      throw new IllegalArgumentException(
          Messages.getFormattedString("ImportCmd.40", //$NON-NLS-1$
                                      new String[] { alias,
                                                     anchorPublicKey.getClass().getName() }));
    if (! sameKey)
      throw new IllegalArgumentException(Messages.getString("ImportCmd.41")); //$NON-NLS-1$

    log.exiting(this.getClass().getName(), "ensureReplyIsOurs"); //$NON-NLS-1$
  }

  private boolean areEqual(DSAPublicKey pk1, DSAPublicKey pk2)
  {
    if (pk1.getY().compareTo(pk2.getY()) != 0)
      return false;

    DSAParams p1 = pk1.getParams();
    DSAParams p2 = pk2.getParams();
    if (p1.getG().compareTo(p2.getG()) != 0)
      return false;

    if (p1.getP().compareTo(p2.getP()) != 0)
      return false;

    return p1.getQ().compareTo(p2.getQ()) == 0;
  }

  private boolean areEqual(RSAPublicKey pk1, RSAPublicKey pk2)
  {
    if (pk1.getPublicExponent().compareTo(pk2.getPublicExponent()) != 0)
      return false;

    return pk1.getModulus().compareTo(pk2.getModulus()) == 0;
  }

  /**
   * @param chain
   * @return the input collection, ordered with own certificate first, and CA's
   *         self-signed certificate last.
   */
  private LinkedList orderChain(Collection chain)
  {
    log.entering(this.getClass().getName(), "orderChain"); //$NON-NLS-1$

    LinkedList result = new LinkedList();
    

    log.entering(this.getClass().getName(), "orderChain", result); //$NON-NLS-1$
    return result;
  }

  /**
   * Given an ordered list of certificates, this method attempts to validate the
   * chain, and if successful, updates the key store entry for the designated
   * alias. The list of certificates is expected to be ordered as a chain, where
   * the first is the alias's own certificate and the last being a self-signed
   * CA certificate.
   * <p>
   * if <code>promptUser</code> is <code>true</code>, then even if no
   * anchor trust certificate is found, the user is prompted to approve, or not,
   * the import operation. On the other hand if the <code>promptUser</code>
   * parameter is <code>false</code> then this method will throw an exception
   * if no trust anchor is to be found.
   * 
   * @param reply an ordered certificate path, where the last entry is the CA's
   *          self-signed certificate.
   * @param promptUser a boolean flag indicating whether or not to prompt the
   *          user for explicit trust in a CA certificate.
   * @return <code>true</code> if the validation succeeds; or <code>false</code>
   *         otherwise.
   * @throws NoSuchAlgorithmException
   * @throws CertPathValidatorException
   * @throws UnsupportedCallbackException
   * @throws IOException
   * @throws UnrecoverableKeyException
   * @throws KeyStoreException
   * @throws CertificateEncodingException
   */
  private boolean findTrustAndUpdate(LinkedList reply, boolean promptUser)
      throws IOException, NoSuchAlgorithmException, CertPathValidatorException,
      KeyStoreException, UnrecoverableKeyException, UnsupportedCallbackException,
      CertificateEncodingException
  {
    log.entering(this.getClass().getName(), "findTrustAndUpdate"); //$NON-NLS-1$

    X509CertPath certPath = new X509CertPath(reply);
    CertPathValidator validator = CertPathValidator.getInstance("PKIX"); //$NON-NLS-1$
    PKIXCertPathValidatorResult cpvr = findTrustInStore(certPath, validator);
    if (cpvr == null && trustCACerts)
      cpvr = findTrustInCACerts(certPath, validator);

    boolean result = false;
    if (cpvr == null)
      {
        if (promptUser)
          {
            printVerbose((Certificate) reply.getLast());
            ConfirmationCallback ccb;
            ccb = new ConfirmationCallback(Messages.getString("ImportCmd.32"), //$NON-NLS-1$
                                           ConfirmationCallback.INFORMATION,
                                           ConfirmationCallback.YES_NO_OPTION,
                                           ConfirmationCallback.NO);
            getCallbackHandler().handle(new Callback[] { ccb });
            int answer = ccb.getSelectedIndex();
            result = answer == ConfirmationCallback.YES;
          }
      }
    else
      {
        log.fine("Found a chain-of-trust anchored by " + cpvr.getTrustAnchor()); //$NON-NLS-1$
        Certificate trustedCert = cpvr.getTrustAnchor().getTrustedCert();
        reply.addLast(trustedCert);
        result = true;
      }

    log.entering(this.getClass().getName(), "findTrustAndUpdate", //$NON-NLS-1$
                 Boolean.valueOf(result));
    return result;
  }

  private PKIXCertPathValidatorResult findTrustInStore(X509CertPath certPath,
                                                       CertPathValidator validator)
  {
    log.entering(this.getClass().getName(), "findTrustInStore"); //$NON-NLS-1$

    PKIXCertPathValidatorResult result;
    try
      {
        PKIXParameters params = new PKIXParameters(store);
        result = (PKIXCertPathValidatorResult) validator.validate(certPath, params);
      }
    catch (Exception x)
      {
        log.log(Level.FINE,
                "Exception in findTrustInStore(). Ignore + Return NULL", //$NON-NLS-1$
                x);
        result = null;
      }

    log.exiting(this.getClass().getName(), "findTrustInStore", result); //$NON-NLS-1$
    return result;
  }

  private PKIXCertPathValidatorResult findTrustInCACerts(X509CertPath certPath,
                                                         CertPathValidator validator)
  {
    log.entering(this.getClass().getName(), "findTrustInCACerts"); //$NON-NLS-1$

    FileInputStream stream = null;
    PKIXCertPathValidatorResult result = null;
    try
      {
        KeyStore cacerts = KeyStore.getInstance("jks"); //$NON-NLS-1$
        String cacertsPath = SystemProperties.getProperty("java.home");
        String fs = SystemProperties.getProperty("file.separator"); //$NON-NLS-1$
        cacertsPath = new StringBuilder(cacertsPath).append(fs)
            .append("lib").append(fs) //$NON-NLS-1$
            .append("security").append(fs) //$NON-NLS-1$
            .append("cacerts").toString(); //$NON-NLS-1$
        stream = new FileInputStream(cacertsPath);
        cacerts.load(stream, "changeit".toCharArray()); //$NON-NLS-1$
        PKIXParameters params = new PKIXParameters(cacerts);
        result = (PKIXCertPathValidatorResult) validator.validate(certPath,
                                                                  params);
      }
    catch (Exception x)
      {
        log.log(Level.FINE,
                "Exception in findTrustInCACerts(). Ignore + Return NULL", //$NON-NLS-1$
                x);
      }
    finally
      {
        if (stream != null)
          try
            {
              stream.close();
            }
          catch (Exception ignored)
            {
            }
      }

    log.exiting(this.getClass().getName(), "findTrustInCACerts", result); //$NON-NLS-1$
    return result;
  }
}
