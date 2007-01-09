/* PrintCertCmd.java -- The printcert command handler of the keytool
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

import java.io.PrintWriter;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.util.logging.Logger;

/**
 * The <b>-printcert</b> keytool command handler is used to read a certificate
 * from a designated file, and print its contents in a human-readable format.
 * <p>
 * Possible options for this command are:
 * <p>
 * <dl>
 *      <dt>-file FILE_NAME</dt>
 *      <dd>The fully qualified path of the file to read the certificate from.
 *      If this option is omitted, the tool will process STDIN.
 *      <p></dd>
 *      
 *      <dt>-v</dt>
 *      <dd>Use this option to enable more verbose output.</dd>
 * </dl>
 */
class PrintCertCmd extends Command
{
  private static final Logger log = Logger.getLogger(PrintCertCmd.class.getName());
  protected String _certFileName;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param pathName the fully qualified path name of the file to process. */
  public void setFile(String pathName)
  {
    this._certFileName = pathName;
  }

  // life-cycle methods -------------------------------------------------------

  void setup() throws Exception
  {
    setInputStreamParam(_certFileName);
    if (Configuration.DEBUG)
      {
        log.fine("-printcert handler will use the following options:"); //$NON-NLS-1$
        log.fine("  -file=" + _certFileName); //$NON-NLS-1$
        log.fine("  -v=" + verbose); //$NON-NLS-1$
      }
  }

  void start() throws CertificateException
  {
    if (Configuration.DEBUG)
      log.entering(getClass().getName(), "start"); //$NON-NLS-1$
    CertificateFactory x509Factory = CertificateFactory.getInstance(Main.X_509);
    Certificate certificate = x509Factory.generateCertificate(inStream);
    PrintWriter writer = new PrintWriter(System.out, true);
    writer.println();
    printVerbose(certificate, writer);
    if (Configuration.DEBUG)
      log.exiting(getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  Parser getParser()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getParser"); //$NON-NLS-1$
    Parser result = new ClasspathToolParser(Main.PRINTCERT_CMD, true);
    result.setHeader(Messages.getString("PrintCertCmd.5")); //$NON-NLS-1$
    result.setFooter(Messages.getString("PrintCertCmd.6")); //$NON-NLS-1$
    OptionGroup options = new OptionGroup(Messages.getString("PrintCertCmd.7")); //$NON-NLS-1$
    options.add(new Option(Main.FILE_OPT,
                           Messages.getString("PrintCertCmd.8"), //$NON-NLS-1$
                           Messages.getString("PrintCertCmd.9")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _certFileName = argument;
      }
    });
    options.add(new Option(Main.VERBOSE_OPT,
                           Messages.getString("PrintCertCmd.10")) //$NON-NLS-1$
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
