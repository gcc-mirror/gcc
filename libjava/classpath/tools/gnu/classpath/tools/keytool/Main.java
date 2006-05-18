/* Main.java -- Implementation of the keytool security tool
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

import gnu.classpath.tools.HelpPrinter;
import gnu.classpath.tools.common.ProviderUtil;
import gnu.java.security.Registry;
import gnu.javax.crypto.jce.GnuCrypto;
import gnu.javax.security.auth.callback.GnuCallbacks;

import java.util.logging.Logger;

/**
 * The GNU Classpath implementation of the keytool security tool.
 * <p>
 * Except for the <code>-identitydb</code> command, available for importing
 * JDK 1.1 <i>identities</i> into a key store, this implementation is intended
 * to be compatible with the behaviour described in the public documentation of
 * the same tool included in JDK 1.4.
 */
public class Main
{
  private static final Logger log = Logger.getLogger(Main.class.getName());
  /** The relative file path to the command tool's help text. */
  private static final String HELP_PATH = "keytool/keytool.txt"; //$NON-NLS-1$
  /** The Preferences key name for the last issued certificate serial nbr. */
  static final String LAST_SERIAL_NUMBER = "lastSerialNumber"; //$NON-NLS-1$
  /** Constant denoting the X.509 certificate type. */
  static final String X_509 = "X.509"; //$NON-NLS-1$

  /** Whether we have already printed the help text or not. */
  private boolean helpPrinted;
  /** The new position of GnuCRYPTO provider if it is not already installed. */
  private int gnuCryptoProviderNdx = -2;
  /** The new position of GNU Callbacks provider if it is not already installed. */
  private int gnuCallbacksNdx = -2;

  private Main()
  {
    super();
  }

  public static final void main(String[] args)
  {
    log.entering(Main.class.getName(), "main", args); //$NON-NLS-1$

    Main tool = new Main();
    try
      {
        tool.setup();
        tool.start(args);
      }
    catch (SecurityException x)
      {
        log.throwing(Main.class.getName(), "main", x); //$NON-NLS-1$
        System.err.println(Messages.getString("Main.6") + x.getMessage()); //$NON-NLS-1$
      }
    catch (Exception x)
      {
        log.throwing(Main.class.getName(), "main", x); //$NON-NLS-1$
        System.err.println(Messages.getString("Main.8") + x); //$NON-NLS-1$
      }
    finally
    {
      tool.teardown();
    }

    log.exiting(Main.class.getName(), "main"); //$NON-NLS-1$
    // System.exit(0);
  }

  // helper methods -----------------------------------------------------------

  private void start(String[] args) throws Exception
  {
    log.entering(this.getClass().getName(), "start", args); //$NON-NLS-1$

    if (args == null)
      args = new String[0];

    int limit = args.length;
    log.finest("args.length=" + limit); //$NON-NLS-1$
    int i = 0;
    String opt;
    Command cmd;
    while (i < limit)
      {
        opt = args[i];
        log.finest("args[" + i + "]=" + opt); //$NON-NLS-1$ //$NON-NLS-2$
        if (opt == null || opt.length() == 0)
          continue;

        cmd = null;
        if ("-genkey".equals(opt)) //$NON-NLS-1$
          cmd = new GenKeyCmd();
        else if ("-import".equals(opt)) //$NON-NLS-1$
          cmd = new ImportCmd();
        else if ("-selfcert".equals(opt)) //$NON-NLS-1$
          cmd = new SelfCertCmd();
        else if ("-identitydb".equals(opt)) //$NON-NLS-1$
          cmd = new IdentityDBCmd();
        else if ("-certreq".equals(opt)) //$NON-NLS-1$
          cmd = new CertReqCmd();
        else if ("-export".equals(opt)) //$NON-NLS-1$
          cmd = new ExportCmd();
        else if ("-list".equals(opt)) //$NON-NLS-1$
          cmd = new ListCmd();
        else if ("-printcert".equals(opt)) //$NON-NLS-1$
          cmd = new PrintCertCmd();
        else if ("-keyclone".equals(opt)) //$NON-NLS-1$
          cmd = new KeyCloneCmd();
        else if ("-storepasswd".equals(opt)) //$NON-NLS-1$
          cmd = new StorePasswdCmd();
        else if ("-keypasswd".equals(opt)) //$NON-NLS-1$
          cmd = new KeyPasswdCmd();
        else if ("-delete".equals(opt)) //$NON-NLS-1$
          cmd = new DeleteCmd();
        else if ("-help".equals(opt)) //$NON-NLS-1$
          {
            printHelp();
            i++;
          }
        else
          {
            log.fine("Unknown command [" + opt + "] at index #" + i //$NON-NLS-1$ //$NON-NLS-2$
                     + ". Arguments from that token onward will be ignored"); //$NON-NLS-1$
            break;
          }

        if (cmd != null)
          {
            i = cmd.processArgs(args, i);
            cmd.doCommand();
          }
      }

    // the -help command is the default; i.e.
    //     keytool
    // is equivalent to:
    //     keytool -help
    if (i == 0)
      printHelp();

    if (i < limit) // more options than needed
      log.fine("Last recognized argument is assumed at index #" + (i - 1) //$NON-NLS-1$
               + ". Remaining arguments (" + args[i] + "...) will be ignored"); //$NON-NLS-1$ //$NON-NLS-2$

    log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  private void setup()
  {
    log.entering(this.getClass().getName(), "setup"); //$NON-NLS-1$

    gnuCryptoProviderNdx = ProviderUtil.addProvider(new GnuCrypto());
    gnuCallbacksNdx = ProviderUtil.addProvider(new GnuCallbacks());

    log.exiting(this.getClass().getName(), "setup"); //$NON-NLS-1$
  }

  private void teardown()
  {
    log.entering(this.getClass().getName(), "teardown"); //$NON-NLS-1$

    // if we added our own providers remove them
    if (gnuCryptoProviderNdx > 0)
      ProviderUtil.removeProvider(Registry.GNU_CRYPTO);

    if (gnuCallbacksNdx > 0)
      ProviderUtil.removeProvider("GNU-CALLBACKS"); //$NON-NLS-1$

    log.exiting(this.getClass().getName(), "teardown"); //$NON-NLS-1$
  }

  private void printHelp()
  {
    if (helpPrinted)
      return;

    HelpPrinter.printHelp(HELP_PATH);
    helpPrinted = true;
  }
}
