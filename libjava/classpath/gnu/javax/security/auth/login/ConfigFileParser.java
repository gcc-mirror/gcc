/* ConfigFileParser.java -- JAAS Login Configuration default syntax parser
   Copyright (C) 2006, 2010  Free Software Foundation, Inc.

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

import gnu.java.security.Configuration;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.security.auth.login.AppConfigurationEntry;

/**
 * A parser that knows how to interpret JAAS Login Module Configuration files
 * written in the <i>default syntax</i> which is interpreted as adhering to
 * the following grammar:
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
 * <p>This parser handles UTF-8 entities when used as APP_NAME and PARAM_VALUE.
 * It also checks for the use of Java identifiers used in MODULE_CLASS, thus
 * minimizing the risks of having {@link java.lang.ClassCastException}s thrown
 * at runtime due to syntactically invalid names.</p>
 *
 * <p>In the above context, a JAVA_IDENTIFIER is a sequence of tokens,
 * separated by the character '.'. Each of these tokens obeys the following:</p>
 *
 * <ol>
 *   <li>its first character yields <code>true</code> when used as an input to
 *   the {@link java.lang.Character#isJavaIdentifierStart(char)}, and</li>
 *   <li>all remaining characters, yield <code>true</code> when used as an
 *   input to {@link java.lang.Character#isJavaIdentifierPart(char)}.</li>
 * </ol>
 */
public final class ConfigFileParser
{
  private static final Logger log = Configuration.DEBUG ?
                Logger.getLogger(ConfigFileParser.class.getName()) : null;

  private ConfigFileTokenizer cft;
  private final Map map = new HashMap();

  // default 0-arguments constructor

  /**
   * Returns the parse result as a {@link Map} where the keys are application
   * names, and the entries are {@link List}s of {@link AppConfigurationEntry}
   * entries, one for each login module entry, in the order they were
   * encountered, for that application name in the just parsed configuration
   * file.
   */
  public Map getLoginModulesMap()
  {
    return map;
  }

  /**
   * Parses the {@link Reader}'s contents assuming it is in the <i>default
   * syntax</i>.
   *
   * @param r the {@link Reader} whose contents are assumed to be a JAAS Login
   * Configuration Module file written in the <i>default syntax</i>.
   * @throws IOException if an exception occurs while parsing the input.
   */
  public void parse(Reader r) throws IOException
  {
    initParser(r);

    while (parseAppOrOtherEntry())
      {
        /* do nothing */
      }
  }

  private void initParser(Reader r) throws IOException
  {
    map.clear();

    cft = new ConfigFileTokenizer(r);
  }

  /**
   * @return <code>true</code> if an APP_OR_OTHER_ENTRY was correctly parsed.
   * Returns <code>false</code> otherwise.
   * @throws IOException if an exception occurs while parsing the input.
   */
  private boolean parseAppOrOtherEntry() throws IOException
  {
    int c = cft.nextToken();
    if (c == ConfigFileTokenizer.TT_EOF)
      return false;

    if (c != ConfigFileTokenizer.TT_WORD)
      {
        cft.pushBack();
        return false;
      }

    String appName = cft.sval;
    if (Configuration.DEBUG)
      log.fine("APP_NAME_OR_OTHER = " + appName);
    if (cft.nextToken() != '{')
      abort("Missing '{' after APP_NAME_OR_OTHER");

    List lmis = new ArrayList();
    while (parseACE(lmis))
      {
        /* do nothing */
      }

    c = cft.nextToken();
    if (c != '}')
      abort("Was expecting '}' but found " + (char) c);

    c = cft.nextToken();
    if (c != ';')
      abort("Was expecting ';' but found " + (char) c);

    List listOfACEs = (List) map.get(appName);
    if (listOfACEs == null)
      {
        listOfACEs = new ArrayList();
        map.put(appName, listOfACEs);
      }
    listOfACEs.addAll(lmis);
    return !appName.equalsIgnoreCase("other");
  }

  /**
   * @return <code>true</code> if a LOGIN_MODULE_ENTRY was correctly parsed.
   * Returns <code>false</code> otherwise.
   * @throws IOException if an exception occurs while parsing the input.
   */
  private boolean parseACE(List listOfACEs) throws IOException
  {
    int c = cft.nextToken();
    if (c != ConfigFileTokenizer.TT_WORD)
      {
        cft.pushBack();
        return false;
      }

    String clazz = validateClassName(cft.sval);
    if (Configuration.DEBUG)
      log.fine("MODULE_CLASS = " + clazz);

    if (cft.nextToken() != ConfigFileTokenizer.TT_WORD)
      abort("Was expecting FLAG but found none");

    String flag = cft.sval;
    if (Configuration.DEBUG)
      log.fine("DEBUG: FLAG = " + flag);
    AppConfigurationEntry.LoginModuleControlFlag f = null;
    if (flag.equalsIgnoreCase("required"))
      f = AppConfigurationEntry.LoginModuleControlFlag.REQUIRED;
    else if (flag.equalsIgnoreCase("requisite"))
      f = AppConfigurationEntry.LoginModuleControlFlag.REQUISITE;
    else if (flag.equalsIgnoreCase("sufficient"))
      f = AppConfigurationEntry.LoginModuleControlFlag.SUFFICIENT;
    else if (flag.equalsIgnoreCase("optional"))
      f = AppConfigurationEntry.LoginModuleControlFlag.OPTIONAL;
    else
      abort("Unknown Flag: " + flag);

    Map options = new HashMap();
    String paramName, paramValue;
    c = cft.nextToken();
    while (c != ';')
      {
        if (c != ConfigFileTokenizer.TT_WORD)
          abort("Was expecting PARAM_NAME but got '" + ((char) c) + "'");

        paramName = cft.sval;
        if (Configuration.DEBUG)
          log.fine("PARAM_NAME = " + paramName);
        if (cft.nextToken() != '=')
          abort("Missing '=' after PARAM_NAME");

        c = cft.nextToken();
        if (c != '"' && c != '\'')
          {
          if (Configuration.DEBUG)
            log.fine("Was expecting a quoted string but got no quote character."
                     + " Assume unquoted string");
          }
        paramValue = expandParamValue(cft.sval);
        if (Configuration.DEBUG)
          log.fine("PARAM_VALUE = " + paramValue);
        options.put(paramName, paramValue);

        c = cft.nextToken();
      }
    AppConfigurationEntry ace = new AppConfigurationEntry(clazz, f, options);
    if (Configuration.DEBUG)
      log.fine("LOGIN_MODULE_ENTRY = " + ace);
    listOfACEs.add(ace);
    return true;
  }

  private void abort(String m) throws IOException
  {
    if (Configuration.DEBUG)
      {
        log.fine(m);
        log.fine("Map (so far) = " + String.valueOf(map));
      }
    throw new IOException(m);
  }

  private String validateClassName(String cn) throws IOException
  {
    if (cn.startsWith(".") || cn.endsWith("."))
      abort("MODULE_CLASS MUST NOT start or end with a '.'");

    String[] tokens = cn.split("\\.");
    for (int i = 0; i < tokens.length; i++)
      {
        String t = tokens[i];
        if (! Character.isJavaIdentifierStart(t.charAt(0)))
          abort("Class name [" + cn
                + "] contains an invalid sub-package identifier: " + t);

        // we dont check the rest of the characters for isJavaIdentifierPart()
        // because that's what the tokenizer does.
      }

    return cn;
  }

  /**
   * The documentation of the {@link javax.security.auth.login.Configuration}
   * states that: <i>"...If a String in the form, ${system.property}, occurs in
   * the value, it will be expanded to the value of the system property."</i>.
   * This method ensures this is the case. If such a string can not be expanded
   * then it is left AS IS, assuming the LoginModule knows what to do with it.
   *
   * <p><b>IMPORTANT</b>: This implementation DOES NOT handle embedded ${}
   * constructs.
   *
   * @param s the raw parameter value, incl. eventually strings of the form
   * <code>${system.property}</code>.
   * @return the input string with every occurence of
   * <code>${system.property}</code> replaced with the value of the
   * corresponding System property at the time of this method invocation. If
   * the string is not a known System property name, then the complete sequence
   * (incl. the ${} characters are passed AS IS.
   */
  private String expandParamValue(String s)
  {
    String result = s;
    try
      {
        int searchNdx = 0;
        while (searchNdx < result.length())
          {
            int i = s.indexOf("${", searchNdx);
            if (i == -1)
              break;

            int j = s.indexOf("}", i + 2);
            if (j == -1)
              {
                if (Configuration.DEBUG)
                  log.fine("Found a ${ prefix with no } suffix. Ignore");
                break;
              }

            String sysPropName = s.substring(i + 2, j);
            if (Configuration.DEBUG)
              log.fine("Found a reference to System property " + sysPropName);
            String sysPropValue = System.getProperty(sysPropName);
            if (Configuration.DEBUG)
              log.fine("Resolved " + sysPropName + " to '" + sysPropValue + "'");
            if (sysPropValue != null)
              {
                result = s.substring(0, i) + sysPropValue + s.substring(j + 1);
                searchNdx = i + sysPropValue.length();
              }
            else
              searchNdx = j + 1;
          }
      }
    catch (Exception x)
      {
        if (Configuration.DEBUG)
          log.fine("Exception (ignored) while expanding " + s + ": " + x);
      }

    return result;
  }
}
