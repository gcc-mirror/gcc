/* ListCmd.java -- The list command handler of the keytool
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
import gnu.java.util.Base64;

import java.io.IOException;
import java.io.PrintWriter;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Enumeration;
import java.util.logging.Logger;

/**
 * The <b>-list</b> keytool command handler is used to output one or all key
 * store entries.
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
 *      <dt>-rfc</dt>
 *      <dd>Use RFC-1421 specifications when encoding the output.
 *      <p></dd>
 *      
 *      <dt>-v</dt>
 *      <dd>Output the certificate in human-readable format. If both this option
 *      and the <code>-rfc</code> option are detected on the command line, the
 *      tool will opt for the human-readable form and will not abort the
 *      command.</dd>
 * </dl>
 */
class ListCmd extends Command
{
  private static final Logger log = Logger.getLogger(ListCmd.class.getName());
  protected String _alias;
  protected String _ksType;
  protected String _ksURL;
  protected String _ksPassword;
  protected String _providerClassName;
  protected boolean rfc;
  private boolean all;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param alias the alias to use. */
  public void setAlias(String alias)
  {
    this._alias = alias;
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
   * @param flag whether to use, or not, RFC-1421 format when listing the
   *          certificate(s).
   */
  public void setRfc(String flag)
  {
    this.rfc = Boolean.valueOf(flag).booleanValue();
  }

  // life-cycle methods -------------------------------------------------------

  void setup() throws Exception
  {
    setOutputStreamParam(null); // use stdout
    setKeyStoreParams(_providerClassName, _ksType, _ksPassword, _ksURL);
    all = _alias == null;
    if (! all)
      setAliasParam(_alias);

    if (verbose & rfc)
      {
        if (Configuration.DEBUG)
          log.fine("Both -v and -rfc options were found on the command line. " //$NON-NLS-1$
                   + "Only the former will be considered"); //$NON-NLS-1$
        rfc = false;
      }
    if (Configuration.DEBUG)
      {
        log.fine("-list handler will use the following options:"); //$NON-NLS-1$
        log.fine("  -alias=" + alias); //$NON-NLS-1$
        log.fine("  -storetype=" + storeType); //$NON-NLS-1$
        log.fine("  -keystore=" + storeURL); //$NON-NLS-1$
        log.fine("  -provider=" + provider); //$NON-NLS-1$
        log.fine("  -v=" + verbose); //$NON-NLS-1$
        log.fine("  -rfc=" + rfc); //$NON-NLS-1$
      }
  }

  void start() throws KeyStoreException, CertificateEncodingException,
      IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$
    PrintWriter writer = new PrintWriter(outStream, true);
    writer.println(Messages.getFormattedString("ListCmd.21", store.getType())); //$NON-NLS-1$
    writer.println(Messages.getFormattedString("ListCmd.22", //$NON-NLS-1$
                                               store.getProvider().getName()));
    if (all)
      {
        if (Configuration.DEBUG)
          log.fine("About to list all aliases in key store..."); //$NON-NLS-1$
        writer.println();
        writer.println(Messages.getFormattedString("ListCmd.24", //$NON-NLS-1$
                                                   Integer.valueOf(store.size())));
        for (Enumeration e = store.aliases(); e.hasMoreElements(); )
          {
            String anAlias = (String) e.nextElement();
            if (anAlias != null)
              list1Alias(anAlias, writer);
          }
      }
    else
      list1Alias(alias, writer);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  Parser getParser()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getParser"); //$NON-NLS-1$
    Parser result = new ClasspathToolParser(Main.LIST_CMD, true);
    result.setHeader(Messages.getString("ListCmd.20")); //$NON-NLS-1$
    result.setFooter(Messages.getString("ListCmd.19")); //$NON-NLS-1$
    OptionGroup options = new OptionGroup(Messages.getString("ListCmd.18")); //$NON-NLS-1$
    options.add(new Option(Main.ALIAS_OPT,
                           Messages.getString("ListCmd.17"), //$NON-NLS-1$
                           Messages.getString("ListCmd.16")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _alias = argument;
      }
    });
    options.add(new Option(Main.STORETYPE_OPT,
                           Messages.getString("ListCmd.15"), //$NON-NLS-1$
                           Messages.getString("ListCmd.14")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksType = argument;
      }
    });
    options.add(new Option(Main.KEYSTORE_OPT,
                           Messages.getString("ListCmd.13"), //$NON-NLS-1$
                           Messages.getString("ListCmd.12")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksURL = argument;
      }
    });
    options.add(new Option(Main.STOREPASS_OPT,
                           Messages.getString("ListCmd.11"), //$NON-NLS-1$
                           Messages.getString("ListCmd.10")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksPassword = argument;
      }
    });
    options.add(new Option(Main.PROVIDER_OPT,
                           Messages.getString("ListCmd.9"), //$NON-NLS-1$
                           Messages.getString("ListCmd.8")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _providerClassName = argument;
      }
    });
    options.add(new Option(Main.VERBOSE_OPT,
                           Messages.getString("ListCmd.7")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        verbose = true;
      }
    });
    options.add(new Option(Main.RFC_OPT,
                           Messages.getString("ListCmd.6")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        rfc = true;
      }
    });
    result.add(options);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getParser", result); //$NON-NLS-1$
    return result;
  }

  /**
   * Prints the certificate(s) associated with the designated alias.
   * 
   * @param anAlias a non-null string denoting an alias in the key-store.
   * @param writer where to print.
   * @throws KeyStoreException if an exception occurs while obtaining the
   *           certificate associated to the designated alias.
   * @throws CertificateEncodingException if an exception occurs while obtaining
   *           the DER encoded form of the certificate.
   * @throws IOException if an I/O related exception occurs during the process.
   */
  private void list1Alias(String anAlias, PrintWriter writer)
      throws KeyStoreException, CertificateEncodingException, IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "list1Alias", anAlias); //$NON-NLS-1$
    writer.println();
    writer.println(Messages.getFormattedString("ListCmd.30", anAlias)); //$NON-NLS-1$
    writer.println(Messages.getFormattedString("ListCmd.31", //$NON-NLS-1$
                                               store.getCreationDate(anAlias)));
    if (store.isCertificateEntry(anAlias))
      {
        writer.println(Messages.getString("ListCmd.32")); //$NON-NLS-1$
        Certificate certificate = store.getCertificate(anAlias);
        print1Certificate(certificate, writer);
      }
    else if (store.isKeyEntry(anAlias))
      {
        writer.println(Messages.getString("ListCmd.33")); //$NON-NLS-1$
        Certificate[] chain = store.getCertificateChain(anAlias);
        print1Chain(chain, writer);
      }
    else
      throw new IllegalArgumentException(Messages.getFormattedString("ListCmd.34", //$NON-NLS-1$
                                                                     anAlias));
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "list1Alias"); //$NON-NLS-1$
  }

  /**
   * Prints the designated certificate chain, or a fingerprint of the first
   * certificate (bottom) in the chain, depending on the values of the flags
   * <code>v</code> (for verbose) and <code>rfc</code>.
   * <p>
   * If both flags are <code>false</code>, only the fingerprint is generated,
   * otherwise, if the <code>v</code> flag is set, then a human readable output
   * is generated. If <code>rfc</code> is set, then an RFC-1421 like output
   * is generated.
   * <p>Note that both <code>v</code> and <code>rfc</code> cannot both be
   * <code>true</code> at the same time.
   * 
   * @param chain the certificate chain to process.
   * @param writer where to print.
   * @throws CertificateEncodingException if an exception occurs while obtaining
   *           the DER encoded form of the certificate.
   */
  private void print1Chain(Certificate[] chain, PrintWriter writer)
      throws CertificateEncodingException
  {
    if (! verbose && ! rfc)
      fingerprint(chain[0], writer);
    else
      {
        int limit = chain.length;
        writer.println(Messages.getFormattedString("ListCmd.38", //$NON-NLS-1$
                                                   Integer.valueOf(limit)));
        writer.println(Messages.getString("ListCmd.39")); //$NON-NLS-1$
        print1Certificate(chain[0], writer);
        for (int i = 1; i < limit; i++)
          {
            writer.println();
            writer.println(Messages.getFormattedString("ListCmd.40", //$NON-NLS-1$
                                                       Integer.valueOf(i + 1)));
            print1Certificate(chain[i], writer);
          }
        writer.println();
        writer.println(Messages.getString("ListCmd.42")); //$NON-NLS-1$
      }
  }

  /**
   * Prints the designated certificate, or its fingerprint, depending on the
   * values of the flags <code>v</code> (for verbose) and <code>rfc</code>.
   * <p>
   * If both flags are <code>false</code>, only a fingerprint is generated,
   * otherwise, if the <code>v</code> flag is set, then a human readable output
   * is generated. If <code>rfc</code> is set, then an RFC-1421 like output
   * is generated.
   * <p>Note that both <code>v</code> and <code>rfc</code> cannot both be
   * <code>true</code> at the same time.
   * 
   * @param certificate the certificate to process.
   * @param writer where to print.
   * @throws CertificateEncodingException if an exception occurs while obtaining
   *           the DER encoded form of the certificate.
   */
  private void print1Certificate(Certificate certificate, PrintWriter writer)
      throws CertificateEncodingException
  {
    if (verbose)
      printVerbose(certificate, writer);
    else if (rfc)
      printRFC1421(certificate, writer);
    else
      fingerprint(certificate, writer);
  }

  private void printRFC1421(Certificate certificate, PrintWriter writer)
      throws CertificateEncodingException
  {
    byte[] derBytes = certificate.getEncoded();
    String encoded = Base64.encode(derBytes, 72);
    writer.println(Messages.getString("ListCmd.43")); //$NON-NLS-1$
    writer.println(encoded);
    writer.println(Messages.getString("ListCmd.44")); //$NON-NLS-1$
  }

  private void fingerprint(Certificate certificate, PrintWriter writer)
      throws CertificateEncodingException
  {
    byte[] derBytes = certificate.getEncoded();
    String fingerPrint = digestWithMD5(derBytes);
    writer.println(Messages.getFormattedString("ListCmd.45", fingerPrint)); //$NON-NLS-1$
  }
}
