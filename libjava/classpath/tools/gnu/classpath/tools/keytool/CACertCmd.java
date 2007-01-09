/* CACertCmd.java -- GNU specific cacert handler
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

import gnu.classpath.Configuration;
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;

import java.io.File;
import java.io.IOException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.util.logging.Logger;

/**
 * The <code>-cacert</code> keytol command handler is used to import a CA
 * trusted X.509 certificate into a key store.
 * <p>
 * Possible options for this command are:
 * <p>
 * <dl>
 *      <dt>-file FILE_NAME</dt>
 *      <dd>The fully qualified path of the file containing the trusted CA
 *      certificate to import. If omitted, the tool will process STDIN.
 *      <p></dd>
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
 *      <dd>Use this option to enable more verbose output.</dd>
 * </dl>
 */
public class CACertCmd
    extends Command
{
  private static final Logger log = Logger.getLogger(CACertCmd.class.getName());
  /** Pathname of the file containing the CA certificate to import. */
  protected String _certFileName;
  /** Type of the key store to use. */
  protected String _ksType;
  /** The URL to the keystore where the trusted certificates will be added. */
  protected String _ksURL;
  /** The password protecting the keystore. */
  protected String _ksPassword;
  /** Class name of a security provider to use. */
  protected String _providerClassName;
  /** Reference to the X.509 factory. */
  private CertificateFactory x509Factory;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param pathName the fully qualified path name of the file to process. */
  public void setFile(String pathName)
  {
    this._certFileName = pathName;
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

  /* (non-Javadoc)
   * @see gnu.classpath.tools.keytool.Command#setup()
   */
  void setup() throws Exception
  {
    setInputStreamParam(_certFileName);
    setKeyStoreParams(_providerClassName, _ksType, _ksPassword, _ksURL);
    if (Configuration.DEBUG)
      {
        log.fine("-cacert handler will use the following options:"); //$NON-NLS-1$
        log.fine("  -file=" + _certFileName); //$NON-NLS-1$
        log.fine("  -storetype=" + storeType); //$NON-NLS-1$
        log.fine("  -keystore=" + storeURL); //$NON-NLS-1$
        log.fine("  -provider=" + provider); //$NON-NLS-1$
        log.fine("  -v=" + verbose); //$NON-NLS-1$
      }
  }

  void start() throws CertificateException, KeyStoreException,
      NoSuchAlgorithmException, IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$
    alias = getAliasFromFileName(_certFileName);
    if (store.containsAlias(alias))
      throw new IllegalArgumentException(Messages.getFormattedString("CACertCmd.0", //$NON-NLS-1$
                                                                     alias));
    x509Factory = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
    Certificate certificate = x509Factory.generateCertificate(inStream);
    if (Configuration.DEBUG)
      log.fine("certificate = " + certificate); //$NON-NLS-1$
    store.setCertificateEntry(alias, certificate);
    saveKeyStore();
    if (verbose)
      System.out.println(Messages.getFormattedString("CACertCmd.1", //$NON-NLS-1$
                                                     new Object[] { _certFileName,
                                                                    alias }));
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  /* (non-Javadoc)
   * @see gnu.classpath.tools.keytool.Command#getParser()
   */
  Parser getParser()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getParser"); //$NON-NLS-1$
    Parser result = new ClasspathToolParser(Main.CACERT_CMD, true);
    result.setHeader(Messages.getString("CACertCmd.2")); //$NON-NLS-1$
    result.setFooter(Messages.getString("CACertCmd.3")); //$NON-NLS-1$
    OptionGroup options = new OptionGroup(Messages.getString("CACertCmd.4")); //$NON-NLS-1$
    options.add(new Option(Main.FILE_OPT,
                           Messages.getString("CACertCmd.5"), //$NON-NLS-1$
                           Messages.getString("CACertCmd.6")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _certFileName = argument;
      }
    });
    options.add(new Option(Main.STORETYPE_OPT,
                           Messages.getString("CACertCmd.7"), //$NON-NLS-1$
                           Messages.getString("CACertCmd.8")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksType = argument;
      }
    });
    options.add(new Option(Main.KEYSTORE_OPT,
                           Messages.getString("CACertCmd.9"), //$NON-NLS-1$
                           Messages.getString("CACertCmd.10")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksURL = argument;
      }
    });
    options.add(new Option(Main.STOREPASS_OPT,
                           Messages.getString("CACertCmd.11"), //$NON-NLS-1$
                           Messages.getString("CACertCmd.12")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksPassword = argument;
      }
    });
    options.add(new Option(Main.PROVIDER_OPT,
                           Messages.getString("CACertCmd.13"), //$NON-NLS-1$
                           Messages.getString("CACertCmd.14")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _providerClassName = argument;
      }
    });
    options.add(new Option(Main.VERBOSE_OPT,
                           Messages.getString("CACertCmd.15")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        verbose = true;
      }
    });
    result.add(options);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getParser", result); //$NON-NLS-1$
    return result;
  }

  /**
   * Construct an Alias string from the name of the file containing the
   * certificate to import. This method first removes the last dot (".")
   * character and any subsequent characters from the input name, and then
   * replaces any space and dot characters with underscores. For example the
   * input string <code>brasil.gov.br.cert</code> will result in
   * <code>brasil_gov_br</code> as its alias.
   * 
   * @param fileName the name of the file containing the CA certificate
   * @return a string which can, and will, be used as the Alias of this CA
   *         certificate.
   */
  private String getAliasFromFileName(String fileName)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getAliasFromFileName", fileName); //$NON-NLS-1$
    // get the basename
    fileName = new File(fileName).getName();
    // remove '.' if at start
    if (fileName.startsWith(".")) //$NON-NLS-1$
      fileName = fileName.substring(1);

    // remove last \..+
    int ndx = fileName.lastIndexOf('.');
    if (ndx > 0)
      fileName = fileName.substring(0, ndx);
    // replace spaces and dots with underscores
    char[] chars = fileName.toCharArray();
    for (int i = 0; i < chars.length; i++)
      {
        char c = chars[i];
        if (c == ' ' || c == '.')
          chars[i] = '_';
      }
    String result = new String(chars);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getAliasFromFileName", result); //$NON-NLS-1$
    return result;
  }
}
