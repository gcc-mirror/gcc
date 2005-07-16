/* PolicyFile.java -- policy file reader
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

package gnu.java.security;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StreamTokenizer;
import java.lang.reflect.Constructor;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.AccessController;
import java.security.CodeSource;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.Permission;
import java.security.PermissionCollection;
import java.security.Permissions;
import java.security.Policy;
import java.security.Principal;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.security.Security;
import java.security.UnresolvedPermission;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * An implementation of a {@link java.security.Policy} object whose
 * permissions are specified by a <em>policy file</em>.
 *
 * <p>The approximate syntax of policy files is:</p>
 *
 * <pre>
 * policyFile ::= keystoreOrGrantEntries ;
 *
 * keystoreOrGrantEntries ::= keystoreOrGrantEntry |
 *                            keystoreOrGrantEntries keystoreOrGrantEntry |
 *                            EMPTY ;
 *
 * keystoreOrGrantEntry ::= keystoreEntry | grantEntry ;
 *
 * keystoreEntry ::= "keystore" keystoreUrl ';' |
 *                   "keystore" keystoreUrl ',' keystoreAlgorithm ';' ;
 *
 * keystoreUrl ::= URL ;
 * keystoreAlgorithm ::= STRING ;
 *
 * grantEntry ::= "grant" domainParameters '{' permissions '}' ';'
 *
 * domainParameters ::= domainParameter |
 *                      domainParameter ',' domainParameters ;
 *
 * domainParameter ::= "signedBy" signerNames |
 *                     "codeBase" codeBaseUrl |
 *                     "principal" principalClassName principalName |
 *                     "principal" principalName ;
 *
 * signerNames ::= quotedString ;
 * codeBaseUrl ::= URL ;
 * principalClassName ::= STRING ;
 * principalName ::= quotedString ;
 *
 * quotedString ::= quoteChar STRING quoteChar ;
 * quoteChar ::= '"' | '\'';
 *
 * permissions ::= permission | permissions permission ;
 *
 * permission ::= "permission" permissionClassName permissionTarget permissionAction |
 *                "permission" permissionClassName permissionTarget |
 *                "permission" permissionClassName;
 * </pre>
 *
 * <p>Comments are either form of Java comments. Keystore entries only
 * affect subsequent grant entries, so if a grant entry preceeds a
 * keystore entry, that grant entry is not affected by that keystore
 * entry. Certian instances of <code>${property-name}</code> will be
 * replaced with <code>System.getProperty("property-name")</code> in
 * quoted strings.</p>
 *
 * <p>This class will load the following files when created or
 * refreshed, in order:</p>
 *
 * <ol>
 * <li>The file <code>${java.home}/lib/security/java.policy</code>.</li>
 * <li>All URLs specified by security properties
 * <code>"policy.file.<i>n</i>"</code>, for increasing <i>n</i>
 * starting from 1. The sequence stops at the first undefined
 * property, so you must set <code>"policy.file.1"</code> if you also
 * set <code>"policy.file.2"</code>, and so on.</li>
 * <li>The URL specified by the property
 * <code>"java.security.policy"</code>.</li>
 * </ol>
 *
 * @author Casey Marshall (csm@gnu.org)
 * @see java.security.Policy
 */
public final class PolicyFile extends Policy
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  private static final boolean DEBUG = true;
  // Package-private to avoid a trampoline.
  static void debug(String msg)
  {
    System.err.print(">> PolicyFile: ");
    System.err.println(msg);
  }

  private static void debug(Throwable t)
  {
    System.err.println(">> PolicyFile");
    t.printStackTrace(System.err);
  }

  private static final String DEFAULT_POLICY = System.getProperty("java.home")
    + System.getProperty("file.separator") + "lib"
    + System.getProperty("file.separator") + "security"
    + System.getProperty("file.separator") + "java.policy";

  private final Map cs2pc;

  // Constructors.
  // -------------------------------------------------------------------------

  public PolicyFile()
  {
    cs2pc = new HashMap();
    refresh();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public PermissionCollection getPermissions(CodeSource codeSource)
  {
    Permissions perms = new Permissions();
    for (Iterator it = cs2pc.entrySet().iterator(); it.hasNext(); )
      {
        Map.Entry e = (Map.Entry) it.next();
        CodeSource cs = (CodeSource) e.getKey();
        if (cs.implies(codeSource))
          {
            if (DEBUG) debug(cs+" -> "+codeSource);
            PermissionCollection pc = (PermissionCollection) e.getValue();
            for (Enumeration ee = pc.elements(); ee.hasMoreElements(); )
              {
                perms.add((Permission) ee.nextElement());
              }
          }
        else
          if (DEBUG) debug(cs+" !-> "+codeSource);
      }
    if (DEBUG) debug ("returning permissions " + perms + " for " + codeSource);
    return perms;
  }

  public void refresh()
  {
    cs2pc.clear();
    List policyFiles = new LinkedList();
    try
      {
        policyFiles.add(new File(DEFAULT_POLICY).toURL());
        if (DEBUG) debug ("defualt policy is " + DEFAULT_POLICY);
        policyFiles.addAll((List) AccessController.doPrivileged(
          new PrivilegedExceptionAction()
          {
            public Object run() throws Exception
            {
              LinkedList l = new LinkedList();
              for (int i = 1; ; i++)
                {
                  String s = Security.getProperty("policy.file."+i);
                  if (DEBUG) debug("policy.file."+i+"="+s);
                  if (s == null)
                    break;
                  l.add(new URL(s));
                }
              String s = System.getProperty("java.security.policy");
              if (DEBUG) debug("java.security.policy="+s);
              if (s != null)
                l.add(new URL(s));
              return l;
            }
          }));
      }
    catch (PrivilegedActionException pae)
      {
        if (DEBUG) debug(pae);
      }
    catch (MalformedURLException mue)
      {
        if (DEBUG) debug(mue);
      }
    for (Iterator it = policyFiles.iterator(); it.hasNext(); )
      {
        try
          {
            URL url = (URL) it.next();
            parse(url);
          }
        catch (IOException ioe)
          {
            if (DEBUG) debug(ioe);
          }
      }
  }

  public String toString()
  {
    return super.toString() + " [ " + cs2pc.toString() + " ]";
  }

  // Own methods.
  // -------------------------------------------------------------------------

  private static final int STATE_BEGIN = 0;
  private static final int STATE_GRANT = 1;
  private static final int STATE_PERMS = 2;

  /**
   * Parse a policy file, incorporating the permission definitions
   * described therein.
   *
   * @param url The URL of the policy file to read.
   * @throws IOException if an I/O error occurs, or if the policy file
   * cannot be parsed.
   */
  private void parse(final URL url) throws IOException
  {
    if (DEBUG) debug ("reading policy file from " + url);
    final StreamTokenizer in = new StreamTokenizer(new InputStreamReader(url.openStream()));
    in.resetSyntax();
    in.slashSlashComments(true);
    in.slashStarComments(true);
    in.wordChars('A', 'Z');
    in.wordChars('a', 'z');
    in.wordChars('0', '9');
    in.wordChars('.', '.');
    in.wordChars('_', '_');
    in.wordChars('$', '$');
    in.whitespaceChars(' ', ' ');
    in.whitespaceChars('\t', '\t');
    in.whitespaceChars('\f', '\f');
    in.whitespaceChars('\n', '\n');
    in.whitespaceChars('\r', '\r');
    in.quoteChar('\'');
    in.quoteChar('"');

    int tok;
    int state = STATE_BEGIN;
    List keystores = new LinkedList();
    URL currentBase = null;
    List currentCerts = new LinkedList();
    Permissions currentPerms = new Permissions();
    while ((tok = in.nextToken()) != StreamTokenizer.TT_EOF)
      {
        switch (tok)
          {
          case '{':
            if (state != STATE_GRANT)
              error(url, in, "spurious '{'");
            state = STATE_PERMS;
            tok = in.nextToken();
            break;
          case '}':
            if (state != STATE_PERMS)
              error(url, in, "spurious '}'");
            state = STATE_BEGIN;
            currentPerms.setReadOnly();
            Certificate[] c = null;
            if (!currentCerts.isEmpty())
              c = (Certificate[]) currentCerts.toArray(new Certificate[currentCerts.size()]);
            cs2pc.put(new CodeSource(currentBase, c), currentPerms);
            currentCerts.clear();
            currentPerms = new Permissions();
            currentBase = null;
            tok = in.nextToken();
            if (tok != ';')
              in.pushBack();
            continue;
          }
        if (tok != StreamTokenizer.TT_WORD)
          {
            error(url, in, "expecting word token");
          }

        // keystore "<keystore-path>" [',' "<keystore-type>"] ';'
        if (in.sval.equalsIgnoreCase("keystore"))
          {
            String alg = KeyStore.getDefaultType();
            tok = in.nextToken();
            if (tok != '"' && tok != '\'')
              error(url, in, "expecting key store URL");
            String store = in.sval;
            tok = in.nextToken();
            if (tok == ',')
              {
                tok = in.nextToken();
                if (tok != '"' && tok != '\'')
                  error(url, in, "expecting key store type");
                alg = in.sval;
                tok = in.nextToken();
              }
            if (tok != ';')
              error(url, in, "expecting semicolon");
            try
              {
                KeyStore keystore = KeyStore.getInstance(alg);
                keystore.load(new URL(url, store).openStream(), null);
                keystores.add(keystore);
              }
            catch (Exception x)
              {
                error(url, in, x.toString());
              }
          }
        else if (in.sval.equalsIgnoreCase("grant"))
          {
            if (state != STATE_BEGIN)
              error(url, in, "extraneous grant keyword");
            state = STATE_GRANT;
          }
        else if (in.sval.equalsIgnoreCase("signedBy"))
          {
            if (state != STATE_GRANT && state != STATE_PERMS)
              error(url, in, "spurious 'signedBy'");
            if (keystores.isEmpty())
              error(url, in, "'signedBy' with no keystores");
            tok = in.nextToken();
            if (tok != '"' && tok != '\'')
              error(url, in, "expecting signedBy name");
            StringTokenizer st = new StringTokenizer(in.sval, ",");
            while (st.hasMoreTokens())
              {
                String alias = st.nextToken();
                for (Iterator it = keystores.iterator(); it.hasNext(); )
                  {
                    KeyStore keystore = (KeyStore) it.next();
                    try
                      {
                        if (keystore.isCertificateEntry(alias))
                          currentCerts.add(keystore.getCertificate(alias));
                      }
                    catch (KeyStoreException kse)
                      {
                        error(url, in, kse.toString());
                      }
                  }
              }
            tok = in.nextToken();
            if (tok != ',')
              {
                if (state != STATE_GRANT)
                  error(url, in, "spurious ','");
                in.pushBack();
              }
          }
        else if (in.sval.equalsIgnoreCase("codeBase"))
          {
            if (state != STATE_GRANT)
              error(url, in, "spurious 'codeBase'");
            tok = in.nextToken();
            if (tok != '"' && tok != '\'')
              error(url, in, "expecting code base URL");
            String base = expand(in.sval);
            if (File.separatorChar != '/')
              base = base.replace(File.separatorChar, '/');
            try
              {
                currentBase = new URL(base);
              }
            catch (MalformedURLException mue)
              {
                error(url, in, mue.toString());
              }
            tok = in.nextToken();
            if (tok != ',')
              in.pushBack();
          }
        else if (in.sval.equalsIgnoreCase("principal"))
          {
            if (state != STATE_GRANT)
              error(url, in, "spurious 'principal'");
            tok = in.nextToken();
            if (tok == StreamTokenizer.TT_WORD)
              {
                tok = in.nextToken();
                if (tok != '"' && tok != '\'')
                  error(url, in, "expecting principal name");
                String name = in.sval;
                Principal p = null;
                try
                  {
                    Class pclass = Class.forName(in.sval);
                    Constructor c =
                      pclass.getConstructor(new Class[] { String.class });
                    p = (Principal) c.newInstance(new Object[] { name });
                  }
                catch (Exception x)
                  {
                    error(url, in, x.toString());
                  }
                for (Iterator it = keystores.iterator(); it.hasNext(); )
                  {
                    KeyStore ks = (KeyStore) it.next();
                    try
                      {
                        for (Enumeration e = ks.aliases(); e.hasMoreElements(); )
                          {
                            String alias = (String) e.nextElement();
                            if (ks.isCertificateEntry(alias))
                              {
                                Certificate cert = ks.getCertificate(alias);
                                if (!(cert instanceof X509Certificate))
                                  continue;
                                if (p.equals(((X509Certificate) cert).getSubjectDN()) ||
                                    p.equals(((X509Certificate) cert).getSubjectX500Principal()))
                                  currentCerts.add(cert);
                              }
                          }
                      }
                    catch (KeyStoreException kse)
                      {
                        error(url, in, kse.toString());
                      }
                  }
              }
            else if (tok == '"' || tok == '\'')
              {
                String alias = in.sval;
                for (Iterator it = keystores.iterator(); it.hasNext(); )
                  {
                    KeyStore ks = (KeyStore) it.next();
                    try
                      {
                        if (ks.isCertificateEntry(alias))
                          currentCerts.add(ks.getCertificate(alias));
                      }
                    catch (KeyStoreException kse)
                      {
                        error(url, in, kse.toString());
                      }
                  }
              }
            else
              error(url, in, "expecting principal");
            tok = in.nextToken();
            if (tok != ',')
              in.pushBack();
          }
        else if (in.sval.equalsIgnoreCase("permission"))
          {
            if (state != STATE_PERMS)
              error(url, in, "spurious 'permission'");
            tok = in.nextToken();
            if (tok != StreamTokenizer.TT_WORD)
              error(url, in, "expecting permission class name");
            String className = in.sval;
            Class clazz = null;
            try
              {
                clazz = Class.forName(className);
              }
            catch (ClassNotFoundException cnfe)
              {
              }
            tok = in.nextToken();
            if (tok == ';')
              {
                if (clazz == null)
                  {
                    currentPerms.add(new UnresolvedPermission(className,
                      null, null, (Certificate[]) currentCerts.toArray(new Certificate[0])));
                    continue;
                  }
                try
                  {
                    currentPerms.add((Permission) clazz.newInstance());
                  }
                catch (Exception x)
                  {
                    error(url, in, x.toString());
                  }
                continue;
              }
            if (tok != '"' && tok != '\'')
              error(url, in, "expecting permission target");
            String target = expand(in.sval);
            tok = in.nextToken();
            if (tok == ';')
              {
                if (clazz == null)
                  {
                    currentPerms.add(new UnresolvedPermission(className,
                      target, null, (Certificate[]) currentCerts.toArray(new Certificate[0])));
                    continue;
                  }
                try
                  {
                    Constructor c =
                      clazz.getConstructor(new Class[] { String.class });
                    currentPerms.add((Permission) c.newInstance(
                      new Object[] { target }));
                  }
                catch (Exception x)
                  {
                    error(url, in, x.toString());
                  }
                continue;
              }
            if (tok != ',')
              error(url, in, "expecting ','");
            tok = in.nextToken();
            if (tok == StreamTokenizer.TT_WORD)
              {
                if (!in.sval.equalsIgnoreCase("signedBy"))
                  error(url, in, "expecting 'signedBy'");
                try
                  {
                    Constructor c =
                      clazz.getConstructor(new Class[] { String.class });
                    currentPerms.add((Permission) c.newInstance(
                      new Object[] { target }));
                  }
                catch (Exception x)
                  {
                    error(url, in, x.toString());
                  }
                in.pushBack();
                continue;
              }
            if (tok != '"' && tok != '\'')
              error(url, in, "expecting permission action");
            String action = in.sval;
            if (clazz == null)
              {
                currentPerms.add(new UnresolvedPermission(className,
                  target, action, (Certificate[]) currentCerts.toArray(new Certificate[0])));
                continue;
              }
            else
              {
                try
                  {
                    Constructor c = clazz.getConstructor(
                      new Class[] { String.class, String.class });
                    currentPerms.add((Permission) c.newInstance(
                      new Object[] { target, action }));
                  }
                catch (Exception x)
                  {
                    error(url, in, x.toString());
                  }
              }
            tok = in.nextToken();
            if (tok != ';' && tok != ',')
              error(url, in, "expecting ';' or ','");
          }
      }
  }

  /**
   * Expand all instances of <code>"${property-name}"</code> into
   * <code>System.getProperty("property-name")</code>.
   */
  private static String expand(final String s)
  {
    final StringBuffer result = new StringBuffer();
    final StringBuffer prop = new StringBuffer();
    int state = 0;
    for (int i = 0; i < s.length(); i++)
      {
        switch (state)
          {
          case 0:
            if (s.charAt(i) == '$')
              state = 1;
            else
              result.append(s.charAt(i));
            break;
          case 1:
            if (s.charAt(i) == '{')
              state = 2;
            else
              {
                state = 0;
                result.append('$').append(s.charAt(i));
              }
            break;
          case 2:
            if (s.charAt(i) == '}')
              {
                String p = prop.toString();
                if (p.equals("/"))
                  p = "file.separator";
                p = System.getProperty(p);
                if (p == null)
                  p = "";
                result.append(p);
                prop.setLength(0);
                state = 0;
              }
            else
              prop.append(s.charAt(i));
            break;
          }
      }
    if (state != 0)
      result.append('$').append('{').append(prop);
    return result.toString();
  }

  /**
   * I miss macros.
   */
  private static void error(URL base, StreamTokenizer in, String msg)
    throws IOException
  {
    throw new IOException(base+":"+in.lineno()+": "+msg);
  }
}
