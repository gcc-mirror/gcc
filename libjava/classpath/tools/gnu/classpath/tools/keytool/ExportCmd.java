/* ExportCmd.java -- The export command handler of the keytool
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
import java.util.logging.Logger;

/**
 * The <b>-export</b> keytool command handler is used to read the certificate
 * associated with a designated alias from the key store, and write it to a
 * designated file.
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
 *      <dd>The fully qualified path of the file where the certificate will be
 *      exported to. If omitted, STDOUT will be used instead.
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
 *      <dd>Output the certificate in binary DER encoding. This is the default
 *      output format of the command if neither <code>-rfc</code> nor
 *      <code>-v</code> options were detected on the command line. If both this
 *      option and the <code>-rfc</code> option are detected on the command
 *      line, the tool will opt for the RFC-1421 style encoding.</dd>
 * </dl>
 */
class ExportCmd extends Command
{
  private static final Logger log = Logger.getLogger(ExportCmd.class.getName());
  protected String _alias;
  protected String _certFileName;
  protected String _ksType;
  protected String _ksURL;
  protected String _ksPassword;
  protected String _providerClassName;
  protected boolean rfc;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param alias the alias to use. */
  public void setAlias(String alias)
  {
    this._alias = alias;
  }

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

  /**
   * @param flag whether to use, or not, RFC-1421 format when exporting the
   *          certificate(s).
   */
  public void setRfc(String flag)
  {
    this.rfc = Boolean.valueOf(flag).booleanValue();
  }

  // life-cycle methods -------------------------------------------------------

  void setup() throws Exception
  {
    setOutputStreamParam(_certFileName);
    setKeyStoreParams(_providerClassName, _ksType, _ksPassword, _ksURL);
    setAliasParam(_alias);
    if (Configuration.DEBUG)
      {
        log.fine("-export handler will use the following options:"); //$NON-NLS-1$
        log.fine("  -alias=" + alias); //$NON-NLS-1$
        log.fine("  -file=" + _certFileName); //$NON-NLS-1$
        log.fine("  -storetype=" + storeType); //$NON-NLS-1$
        log.fine("  -keystore=" + storeURL); //$NON-NLS-1$
        log.fine("  -provider=" + provider); //$NON-NLS-1$
        log.fine("  -rfc=" + rfc); //$NON-NLS-1$
        log.fine("  -v=" + verbose); //$NON-NLS-1$
      }
  }

  void start() throws KeyStoreException, CertificateEncodingException,
      IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$
    ensureStoreContainsAlias();
    Certificate certificate;
    if (store.isCertificateEntry(alias))
      {
        if (Configuration.DEBUG)
          log.fine("Alias [" + alias + "] is a trusted certificate"); //$NON-NLS-1$ //$NON-NLS-2$
        certificate = store.getCertificate(alias);
      }
    else
      {
        if (Configuration.DEBUG)
          log.fine("Alias [" + alias + "] is a key entry"); //$NON-NLS-1$ //$NON-NLS-2$
        Certificate[] chain = store.getCertificateChain(alias);
        certificate = chain[0];
      }

    byte[] derBytes = certificate.getEncoded();
    if (rfc)
      {
        String encoded = Base64.encode(derBytes, 72);
        PrintWriter pw = new PrintWriter(outStream, true);
        pw.println("-----BEGIN CERTIFICATE-----"); //$NON-NLS-1$
        pw.println(encoded);
        pw.println("-----END CERTIFICATE-----"); //$NON-NLS-1$
      }
    else
      outStream.write(derBytes);

    // stream is closed in Command.teardown()
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  Parser getParser()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getParser"); //$NON-NLS-1$
    Parser result = new ClasspathToolParser(Main.EXPORT_CMD, true);
    result.setHeader(Messages.getString("ExportCmd.17")); //$NON-NLS-1$
    result.setFooter(Messages.getString("ExportCmd.18")); //$NON-NLS-1$
    OptionGroup options = new OptionGroup(Messages.getString("ExportCmd.19")); //$NON-NLS-1$
    options.add(new Option(Main.ALIAS_OPT,
                           Messages.getString("ExportCmd.20"), //$NON-NLS-1$
                           Messages.getString("ExportCmd.21")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _alias = argument;
      }
    });
    options.add(new Option(Main.FILE_OPT,
                           Messages.getString("ExportCmd.22"), //$NON-NLS-1$
                           Messages.getString("ExportCmd.23")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _certFileName = argument;
      }
    });
    options.add(new Option(Main.STORETYPE_OPT,
                           Messages.getString("ExportCmd.24"), //$NON-NLS-1$
                           Messages.getString("ExportCmd.25")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksType = argument;
      }
    });
    options.add(new Option(Main.KEYSTORE_OPT,
                           Messages.getString("ExportCmd.26"), //$NON-NLS-1$
                           Messages.getString("ExportCmd.27")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksURL = argument;
      }
    });
    options.add(new Option(Main.STOREPASS_OPT,
                           Messages.getString("ExportCmd.28"), //$NON-NLS-1$
                           Messages.getString("ExportCmd.29")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksPassword = argument;
      }
    });
    options.add(new Option(Main.PROVIDER_OPT,
                           Messages.getString("ExportCmd.30"), //$NON-NLS-1$
                           Messages.getString("ExportCmd.31")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _providerClassName = argument;
      }
    });
    options.add(new Option(Main.RFC_OPT,
                           Messages.getString("ExportCmd.32")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        rfc = true;
      }
    });
    options.add(new Option(Main.VERBOSE_OPT,
                           Messages.getString("ExportCmd.33")) //$NON-NLS-1$
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
}
