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

import gnu.classpath.Configuration;
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.common.ProviderUtil;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;
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
  static final String KEYTOOL_TOOL = "keytool"; //$NON-NLS-1$
  static final String GENKEY_CMD = "genkey"; //$NON-NLS-1$
  static final String IMPORT_CMD = "import"; //$NON-NLS-1$
  static final String SELFCERT_CMD = "selfcert"; //$NON-NLS-1$
  static final String IDENTITYDB_CMD = "identitydb"; //$NON-NLS-1$
  static final String CERTREQ_CMD = "certreq"; //$NON-NLS-1$
  static final String EXPORT_CMD = "export"; //$NON-NLS-1$
  static final String LIST_CMD = "list"; //$NON-NLS-1$
  static final String PRINTCERT_CMD = "printcert"; //$NON-NLS-1$
  static final String KEYCLONE_CMD = "keyclone"; //$NON-NLS-1$
  static final String STOREPASSWD_CMD = "storepasswd"; //$NON-NLS-1$
  static final String KEYPASSWD_CMD = "keypasswd"; //$NON-NLS-1$
  static final String DELETE_CMD = "delete"; //$NON-NLS-1$
  static final String CACERT_CMD = "cacert"; //$NON-NLS-1$

  static final String _GENKEY = "-" + GENKEY_CMD; //$NON-NLS-1$
  static final String _IMPORT = "-" + IMPORT_CMD; //$NON-NLS-1$
  static final String _SELFCERT = "-" + SELFCERT_CMD; //$NON-NLS-1$
  static final String _IDENTITYDB = "-" + IDENTITYDB_CMD; //$NON-NLS-1$
  static final String _CERTREQ = "-" + CERTREQ_CMD; //$NON-NLS-1$
  static final String _EXPORT = "-" + EXPORT_CMD; //$NON-NLS-1$
  static final String _LIST = "-" + LIST_CMD; //$NON-NLS-1$
  static final String _PRINTCERT = "-" + PRINTCERT_CMD; //$NON-NLS-1$
  static final String _KEYCLONE = "-" + KEYCLONE_CMD; //$NON-NLS-1$
  static final String _STOREPASSWD = "-" + STOREPASSWD_CMD; //$NON-NLS-1$
  static final String _KEYPASSWD = "-" + KEYPASSWD_CMD; //$NON-NLS-1$
  static final String _DELETE = "-" + DELETE_CMD; //$NON-NLS-1$
  static final String _HELP = "-help"; //$NON-NLS-1$
  static final String _CACERT = "-" + CACERT_CMD; //$NON-NLS-1$

  static final String ALIAS_OPT = "alias"; //$NON-NLS-1$
  static final String SIGALG_OPT = "sigalg"; //$NON-NLS-1$
  static final String KEYALG_OPT = "keyalg"; //$NON-NLS-1$
  static final String KEYSIZE_OPT = "keysize"; //$NON-NLS-1$
  static final String KEYPASS_OPT = "keypass"; //$NON-NLS-1$
  static final String VALIDITY_OPT = "validity"; //$NON-NLS-1$
  static final String STORETYPE_OPT = "storetype"; //$NON-NLS-1$
  static final String STOREPASS_OPT = "storepass"; //$NON-NLS-1$
  static final String KEYSTORE_OPT = "keystore"; //$NON-NLS-1$
  static final String PROVIDER_OPT = "provider"; //$NON-NLS-1$
  static final String FILE_OPT = "file"; //$NON-NLS-1$
  static final String VERBOSE_OPT = "v"; //$NON-NLS-1$
  static final String DEST_OPT = "dest"; //$NON-NLS-1$
  static final String NEW_OPT = "new"; //$NON-NLS-1$
  static final String RFC_OPT = "rfc"; //$NON-NLS-1$
  static final String DNAME_OPT = "dname"; //$NON-NLS-1$

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
  /** The command line parser. */
  private Parser cmdLineParser;
  /** The shutdown hook. */
  private ShutdownHook shutdownThread;

  private Main()
  {
    super();
    shutdownThread = new ShutdownHook();
    Runtime.getRuntime().addShutdownHook(shutdownThread);
  }

  public static final void main(String[] args)
  {
    if (Configuration.DEBUG)
      log.entering(Main.class.getName(), "main", args); //$NON-NLS-1$
    Main tool = new Main();
    int result = 1;
    try
      {
        tool.setup();
        tool.start(args);
        result = 0;
      }
    catch (OptionException x)
      {
        System.err.println(x.getMessage());
        if (tool.cmdLineParser != null)
          tool.cmdLineParser.printHelp();
      }
    catch (SecurityException x)
      {
        if (Configuration.DEBUG)
          log.throwing(Main.class.getName(), "main", x); //$NON-NLS-1$
        System.err.println(Messages.getFormattedString("Main.6", //$NON-NLS-1$
                                                       x.getMessage()));
      }
    catch (Exception x)
      {
        if (Configuration.DEBUG)
          log.throwing(Main.class.getName(), "main", x); //$NON-NLS-1$
        System.err.println(Messages.getFormattedString("Main.8", x)); //$NON-NLS-1$
      }
    finally
      {
        tool.teardown();
        if (tool.shutdownThread != null)
          Runtime.getRuntime().removeShutdownHook(tool.shutdownThread);
      }
    if (Configuration.DEBUG)
      log.exiting(Main.class.getName(), "main", Integer.valueOf(result)); //$NON-NLS-1$
    System.exit(result);
  }

  // helper methods -----------------------------------------------------------

  private void setup()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "setup"); //$NON-NLS-1$
    cmdLineParser = getParser();
    gnuCryptoProviderNdx = ProviderUtil.addProvider(new GnuCrypto());
    gnuCallbacksNdx = ProviderUtil.addProvider(new GnuCallbacks());
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "setup"); //$NON-NLS-1$
  }

  private void start(String[] args) throws Exception
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$
    if (args == null || args.length == 0)
      throw new OptionException(""); //$NON-NLS-1$

    String opt;
    Command cmd;
    while (args.length > 0)
      {
        opt = args[0];
        cmd = null;
        if (_GENKEY.equals(opt))
          cmd = new GenKeyCmd();
        else if (_IMPORT.equals(opt))
          cmd = new ImportCmd();
        else if (_SELFCERT.equals(opt))
          cmd = new SelfCertCmd();
        else if (_IDENTITYDB.equals(opt))
          cmd = new IdentityDBCmd();
        else if (_CERTREQ.equals(opt))
          cmd = new CertReqCmd();
        else if (_EXPORT.equals(opt))
          cmd = new ExportCmd();
        else if (_LIST.equals(opt))
          cmd = new ListCmd();
        else if (_PRINTCERT.equals(opt))
          cmd = new PrintCertCmd();
        else if (_KEYCLONE.equals(opt))
          cmd = new KeyCloneCmd();
        else if (_STOREPASSWD.equals(opt))
          cmd = new StorePasswdCmd();
        else if (_KEYPASSWD.equals(opt))
          cmd = new KeyPasswdCmd();
        else if (_DELETE.equals(opt))
          cmd = new DeleteCmd();
        else if (_CACERT.equals(opt))
          cmd = new CACertCmd();
        else if (_HELP.equals(opt))
          throw new OptionException(""); //$NON-NLS-1$
        else
          throw new OptionException(Messages.getFormattedString("Main.18", //$NON-NLS-1$
                                                                opt));

        String[] cmdArgs = new String[args.length - 1];
        System.arraycopy(args, 1, cmdArgs, 0, cmdArgs.length);
        args = cmd.processArgs(cmdArgs);
        cmd.doCommand();
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  private Parser getParser()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getParser"); //$NON-NLS-1$
    Parser result = new ClasspathToolParser(KEYTOOL_TOOL, true);
    result.setHeader(Messages.getString("Main.19")); //$NON-NLS-1$
    result.setFooter(Messages.getString("Main.20")); //$NON-NLS-1$
    OptionGroup cmdGroup = new OptionGroup(Messages.getString("Main.21")); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(GENKEY_CMD,
                                   Messages.getString("Main.22"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(IMPORT_CMD,
                                   Messages.getString("Main.23"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(SELFCERT_CMD,
                                   Messages.getString("Main.24"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(IDENTITYDB_CMD,
                                   Messages.getString("Main.25"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(CERTREQ_CMD,
                                   Messages.getString("Main.26"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(EXPORT_CMD,
                                   Messages.getString("Main.27"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(LIST_CMD,
                                   Messages.getString("Main.28"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(PRINTCERT_CMD,
                                   Messages.getString("Main.29"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(KEYCLONE_CMD,
                                   Messages.getString("Main.30"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(STOREPASSWD_CMD,
                                   Messages.getString("Main.31"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(KEYPASSWD_CMD,
                                   Messages.getString("Main.32"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(DELETE_CMD,
                                   Messages.getString("Main.33"))); //$NON-NLS-1$
    cmdGroup.add(new NoParseOption(CACERT_CMD,
                                   Messages.getString("Main.5"))); //$NON-NLS-1$
    result.add(cmdGroup);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getParser", result); //$NON-NLS-1$
    return result;
  }

  void teardown()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "teardown"); //$NON-NLS-1$
    // if we added our own providers remove them
    if (gnuCryptoProviderNdx > 0)
      ProviderUtil.removeProvider(Registry.GNU_CRYPTO);

    if (gnuCallbacksNdx > 0)
      ProviderUtil.removeProvider("GNU-CALLBACKS"); //$NON-NLS-1$

    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "teardown"); //$NON-NLS-1$
  }

  // Inner class(es)
  // ==========================================================================

  private class NoParseOption
      extends Option
  {
    public NoParseOption(String name, String description)
    {
      super(name, description);
    }

    public NoParseOption(String name, String description, String param)
    {
      super(name, description, param);
    }

    public void parsed(String argument) throws OptionException
    {
      // do nothing
    }
  }

  private class ShutdownHook
      extends Thread
  {
    public void run()
    {
      teardown();
    }
  }
}
