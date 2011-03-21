/* GnuConfiguration.java -- GNU Classpath implementation of JAAS Configuration
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


package gnu.javax.security.auth.login;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.Security;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.security.auth.AuthPermission;
import javax.security.auth.login.AppConfigurationEntry;
import javax.security.auth.login.Configuration;

/**
 * An implementation of the {@link Configuration} class which interprets JAAS
 * Login Configuration files written in the <i>default</i> syntax described in
 * the publicly available documentation of that class. A more formal definition
 * of this syntax is as follows:
 *
 * <pre>
 *   CONFIG              ::= APP_OR_OTHER_ENTRY+
 *   APP_OR_OTHER_ENTRY  ::= APP_NAME_OR_OTHER JAAS_CONFIG_BLOCK
 *   APP_NAME_OR_OTHER   ::= APP_NAME
 *                         | 'other'
 *   JAAS_CONFIG_BLOCK   ::= '{' (LOGIN_MODULE_ENTRY ';')+ '}' ';'
 *   LOGIN_MODULE_ENTRY  ::= MODULE_CLASS FLAG MODULE_OPTION* ';'
 *   FLAG                ::= 'required'
 *                         | 'requisite'
 *                         | 'sufficient'
 *                         | 'optional'
 *   MODULE_OPTION       ::= PARAM_NAME '=' PARAM_VALUE
 *
 *   APP_NAME     ::= JAVA_IDENTIFIER
 *   MODULE_CLASS ::= JAVA_IDENTIFIER ('.' JAVA_IDENTIFIER)*
 *   PARAM_NAME   ::= STRING
 *   PARAM_VALUE  ::= '"' STRING '"' | ''' STRING ''' | STRING
 * </pre>
 *
 * <p>This implementation will specifically attempt to process one or more
 * Login Configuration files in the following locations, and when found parse
 * them and merge their contents. The locations, and the order in which they are
 * investigated, follows:</p>
 *
 * <ol>
 *   <li>If the following Security properties:
 *   <i>java.security.auth.login.config.url.<b>N</b></i>, where <i><b>N</b></i>
 *   is a digit, from <code>1</code> to an arbitrary number, are defined, then
 *   the value of each of those properties will be considered as a JAAS Login
 *   Configuration file written in the default syntax. This implementation will
 *   attempt parsing all such files.
 *
 *   <p>It is worth noting the following:
 *     <ul>
 *       <li>The GNU Classpath security file, named <i>classpath.security</i>,
 *       where all Security properties are encoded, is usually located in
 *       <i>/usr/local/classpath/lib/security</i> folder.</li>
 *
 *       <li>The numbers used in the properties
 *       <i>java.security.auth.login.config.url.<b>N</b></i> MUST be sequential,
 *       with no breaks in-between.</li>
 *     </ul>
 *   </p>
 *
 *   <p>If at least one of the designated Configuration files was found, and
 *   was parsed correctly, then no other location will be inspected.</p></li>
 *
 *   <li>If the System property named <i>java.security.auth.login.config</i>
 *   is not null or empty, its contents are then interpreted as a URL to a
 *   JAAS Login Configuration file written in the default syntax.
 *
 *   <p>If this System property is defined, and the file it refers to was
 *   parsed correctly, then no other location will be inspected.</p></li>
 *
 *   <li>If a file named <i>.java.login.config</i> or <i>java.login.config</i>
 *   (in that order) is found in the location referenced by the value of the
 *   System property <i>user.home</i>, then that file is parsed as a JAAS Login
 *   Configuration written in the default syntax.</li>
 *
 *   <li>If none of the above resulted in a correctly parsed JAAS Login
 *   Configuration file, then this implementation will install a <i>Null
 *   Configuration</i> which basically does not recognize any Application.</li>
 * </ol>
 */
public final class GnuConfiguration extends Configuration
{
  private static final Logger log = Logger.getLogger(GnuConfiguration.class.getName());
  /**
   * The internal map of login modules keyed by application name. Each entry in
   * this map is a {@link List} of {@link AppConfigurationEntry}s for that
   * application name.
   */
  private Map loginModulesMap;
  /** Our reference to our default syntax parser. */
  private ConfigFileParser cp;

  // Constructor(s)
  // --------------------------------------------------------------------------

  /** Trivial 0-arguments Constructor. */
  public GnuConfiguration()
  {
    super();

    loginModulesMap = new HashMap();
    cp = new ConfigFileParser();
    init();
  }

  // Class methods
  // --------------------------------------------------------------------------

  // Instance methods
  // --------------------------------------------------------------------------

  // Configuration abstract methods implementation ----------------------------

  /* (non-Javadoc)
   * @see javax.security.auth.login.Configuration#getAppConfigurationEntry(java.lang.String)
   */
  public AppConfigurationEntry[] getAppConfigurationEntry(String appName)
  {
    if (appName == null)
      return null;

    appName = appName.trim();
    if (appName.length() == 0)
      return null;

    List loginModules = (List) loginModulesMap.get(appName);
    if (loginModules == null || loginModules.size() == 0)
      return null;

    if (gnu.java.security.Configuration.DEBUG)
      log.fine(appName + " -> " + loginModules.size() + " entry(ies)");
    return (AppConfigurationEntry[]) loginModules.toArray(new AppConfigurationEntry[0]);
  }

  /**
   * Refreshes and reloads this <code>Configuration</code>.
   *
   * <p>This method causes this <code>Configuration</code> object to refresh /
   * reload its contents following the locations and logic described above in
   * the class documentation section.</p>
   *
   * @throws SecurityException if the caller does not have an
   * {@link AuthPermission} for the action named
   * <code>refreshLoginConfiguration</code>.
   * @see AuthPermission
   */
  public void refresh()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new AuthPermission("refreshLoginConfiguration"));

    loginModulesMap.clear();
    init();
  }

  // helper methods -----------------------------------------------------------

  /**
   * Attempts to find and parse JAAS Login Configuration file(s) written in
   * the default syntax. The locations searched are as descibed in the class
   * documentation.
   */
  private void init()
  {
    if (processSecurityProperties())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("Using login configuration defined by Security property(ies)");
      }
    else if (processSystemProperty())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("Using login configuration defined by System property");
      }
    else if (processUserHome())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("Using login configuration defined in ${user.home}");
      }
    else
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("No login configuration file found");
      }
  }

  /**
   * Attempts to locate and parse one or more JAAS Login Configuration files
   * defined as the values of the Security properties
   * <i>java.security.auth.login.config.url.N</i>.
   *
   * @return <code>true</code> if it succeeds, and <code>false</code>
   *         otherwsie.
   */
  private boolean processSecurityProperties()
  {
    boolean result = false;
    int counter = 0;
    String s;
    while (true)
      try
        {
          counter++;
          s = Security.getProperty("java.security.auth.login.config.url."
                                   + counter);
          if (s == null)
            break;

          s = s.trim();
          if (s.length() != 0)
            {
              if (gnu.java.security.Configuration.DEBUG)
                log.fine("java.security.auth.login.config.url." + counter
                         + " = " + s);
              parseConfig(getInputStreamFromURL(s));
              result = true;
            }
        }
      catch (Throwable t)
        {
          if (gnu.java.security.Configuration.DEBUG)
            log.fine("Exception while handling Security property at #"
                     + counter + ". Continue: " + t);
        }
    return result;
  }

  /**
   * Attempts to open a designated string as a well-formed {@link URL}. If a
   * {@link MalformedURLException} occurs, this method then tries to open that
   * string as a {@link File} (with the same name). If it succeeds, an
   * {@link InputStream} is constructed and returned.
   *
   * @param s
   *          the designated name of either a {@link URL} or a {@link File}
   *          assumed to contain a JAAS Login Configuration in the default
   *          syntax.
   * @return an {@link InputStream} around the data source.
   * @throws IOException
   *           if an exception occurs during the operation.
   */
  private InputStream getInputStreamFromURL(String s) throws IOException
  {
    InputStream result = null;
    try
      {
        URL url = new URL(s);
        result = url.openStream();
      }
    catch (MalformedURLException x)
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("Failed opening as URL: " + s + ". Will try as File");
        result = new FileInputStream(s);
      }
    return result;
  }

  /**
   * Attempts to locate and parse a JAAS Login Configuration file defined as the
   * value of the System property <i>java.security.auth.login.config</i>.
   *
   * @return <code>true</code> if it succeeds, and <code>false</code>
   *         otherwsie.
   */
  private boolean processSystemProperty()
  {
    boolean result = false;
    try
      {
        String s = System.getProperty("java.security.auth.login.config");
        if (s != null)
          {
            s = s.trim();
            if (s.length() != 0)
              {
                if (gnu.java.security.Configuration.DEBUG)
                  log.fine("java.security.auth.login.config = " + s);
                parseConfig(getInputStreamFromURL(s));
                result = true;
              }
          }
      }
    catch (Throwable t)
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("Exception while handling System property. Continue: " + t);
      }
    return result;
  }

  /**
   * Attempts to locate and parse a JAAS Login Configuration file named either
   * as <i>.java.login.config</i> or <i>java.login.config</i> (without the
   * leading dot) in the folder referenced by the System property
   * <code>user.home</code>.
   *
   * @return <code>true</code> if it succeeds, and <code>false</code>
   *         otherwsie.
   */
  private boolean processUserHome()
  {
    boolean result = false;
    try
      {
        File userHome = getUserHome();
        if (userHome == null)
          return result;

        File jaasFile;
        jaasFile = getConfigFromUserHome(userHome, ".java.login.config");
        if (jaasFile == null)
          jaasFile = getConfigFromUserHome(userHome, "java.login.config");

        if (jaasFile == null)
          {
            if (gnu.java.security.Configuration.DEBUG)
              log.fine("Login Configuration file, in " + userHome
                       + ", does not exist or is inaccessible");
            return result;
          }

        FileInputStream fis = new FileInputStream(jaasFile);
        parseConfig(fis);
        result = true;
      }
    catch (Throwable t)
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("Exception (ignored) while handling ${user.home}: " + t);
      }
    return result;
  }

  private void parseConfig(InputStream configStream) throws IOException
  {
    cp.parse(new InputStreamReader(configStream, "UTF-8"));
    Map loginModulesMap = cp.getLoginModulesMap();
    mergeLoginModules(loginModulesMap);
  }

  private void mergeLoginModules(Map otherLoginModules)
  {
    if (otherLoginModules == null || otherLoginModules.size() < 1)
      return;

    for (Iterator it = otherLoginModules.keySet().iterator(); it.hasNext();)
      {
        String appName = (String) it.next();
        List thatListOfACEs = (List) otherLoginModules.get(appName);
        if (thatListOfACEs == null || thatListOfACEs.size() < 1)
          continue;

        List thisListsOfACEs = (List) loginModulesMap.get(appName);
        if (thisListsOfACEs == null)
          loginModulesMap.put(appName, thatListOfACEs);
        else
          thisListsOfACEs.addAll(thatListOfACEs);
      }
  }

  private File getUserHome()
  {
    String uh = System.getProperty("user.home");
    if (uh == null || uh.trim().length() == 0)
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("User home path is not set or is empty");
        return null;
      }
    uh = uh.trim();
    File result = new File(uh);
    if (! result.exists())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("User home '" + uh + "' does not exist");
        return null;
      }
    if (! result.isDirectory())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("User home '" + uh + "' is not a directory");
        return null;
      }
    if (! result.canRead())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("User home '" + uh + "' is not readable");
        return null;
      }
    return result;
  }

  private File getConfigFromUserHome(File userHome, String fileName)
  {
    File result = new File(userHome, fileName);
    if (! result.exists())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("File '" + fileName + "' does not exist in user's home");
        return null;
      }
    if (! result.isFile())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("File '" + fileName + "' in user's home is not a file");
        return null;
      }
    if (! result.canRead())
      {
        if (gnu.java.security.Configuration.DEBUG)
          log.fine("File '" + fileName + "' in user's home is not readable");
        return null;
      }
    return result;
  }
}
