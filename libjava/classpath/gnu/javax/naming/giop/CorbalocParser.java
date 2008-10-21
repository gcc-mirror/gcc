/* CorbalocParser.java -- handles corbaname: urls
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

package gnu.javax.naming.giop;

import gnu.CORBA.IOR;
import gnu.CORBA.Minor;
import gnu.CORBA.Unexpected;
import gnu.CORBA.Version;
import gnu.CORBA.NamingService.NameTransformer;

import gnu.java.lang.CPStringBuilder;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.StringTokenizer;

import javax.naming.InvalidNameException;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.DATA_CONVERSION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.ORBPackage.InvalidName;

/**
 * Parses the alternative IOR representations into our IOR structure.
 * 
 * TODO This parser currently supports only one address per target string. A
 * string with the multiple addresses will be accepted, but only the last
 * address will be taken into consideration. The fault tolerance is not yet
 * implemented.
 * 
 * The key string is filtered using {@link java.net.URLDecoder} that replaces
 * the agreed escape sequences by the corresponding non alphanumeric characters.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class CorbalocParser
  extends NameTransformer
{
  /**
   * The corbaloc prefix.
   */
  public static final String pxCORBALOC = "corbaloc";

  /**
   * The corbaname prefix.
   */
  public static final String pxCORBANAME = "corbaname";

  /**
   * The IOR prefix.
   */
  public static final String pxIOR = "ior";
  
  /**
   * The file:// prefix.
   */
  public static final String pxFILE = "file://";
  
  /**
   * The ftp:// prefix.
   */
  public static final String pxFTP = "ftp://";
  
  /**
   * The http:// prefix.
   */
  public static final String pxHTTP = "http://";

  /**
   * Marks iiop protocol.
   */
  public static final String IIOP = "iiop";

  /**
   * Marks rir protocol.
   */
  public static final String RIR = "rir";

  /**
   * The default port value, as specified in OMG documentation.
   */
  public static final int DEFAULT_PORT = 2809;

  /**
   * The default name.
   */
  public static final String DEFAULT_NAME = "NameService";

  /**
   * The string to name converter, initialized on demand.
   */
  static NameTransformer converter;

  /**
   * The current position.
   */
  int p;

  /**
   * The address being parsed, splitted into tokens.
   */
  String[] t;

  /**
   * Parse CORBALOC.
   * 
   * The expected format is: <br>
   * 1. corbaloc:[iiop][version.subversion@]:host[:port]/key <br>
   * 2. corbaloc:rir:[/key] <br>
   * 3. corbaname:[iiop][version.subversion@]:host[:port]/key <br>
   * 4. corbaname:rir:[/key] <br>
   * 5. file://[file name]<br>
   * 6. http://[url]<br>
   * 7. ftp://[url]<br>
   * 
   * Protocol defaults to IOP, the object key defaults to the NameService.
   * 
   * @param corbaloc the string to parse.
   * @param orb the ORB, needed to create IORs and resolve rir references.
   * 
   * @return the arrey of strings, first member being the IOR of the
   * naming service, second member the name in the naming service. 
   */
  public synchronized String[] corbaloc(String corbaloc,
    ORB orb)
    throws InvalidNameException
  {
    return corbaloc(corbaloc, orb, 0);
  }
  
  /**
   * Parse controlling against the infinite recursion loop.
   */
  private String[] corbaloc(String corbaloc,
    ORB orb, int recursion) throws InvalidNameException
  {
    // The used CORBA specification does not state how many times we should to
    //redirect, but the infinite loop may be used to knock out the system.
    // by malicious attempt.
    if (recursion > 10)
      throw new DATA_CONVERSION("More than 10 redirections");
    
    if (corbaloc.startsWith(pxFILE))
      return corbaloc(readFile(corbaloc.substring(pxFILE.length())), orb, recursion+1);
    else if (corbaloc.startsWith(pxHTTP))
      return corbaloc(readUrl(corbaloc), orb, recursion+1);
    else if (corbaloc.startsWith(pxFTP))
      return corbaloc(readUrl(corbaloc), orb, recursion+1);

    // The version numbers with default values.
    int major = 1;
    int minor = 0;

    // The host address.
    String host;

    // The port.
    int port = DEFAULT_PORT;

    // The object key as string.
    String key;

    StringTokenizer st = new StringTokenizer(corbaloc, ":@/.,#", true);

    t = new String[st.countTokens()];

    for (int i = 0; i < t.length; i++)
      {
        t[i] = st.nextToken();
      }

    p = 0;

    if (!t[p].startsWith(pxCORBANAME))
      throw new InvalidNameException(corbaloc+" must start with "+pxCORBANAME);

    p++;

    if (!t[p++].equals(":"))
      throw new BAD_PARAM("Syntax (':' expected after name prefix)");

    // Check for rir:
    if (t[p].equals(RIR))
      {
        p++;
        if (!t[p++].equals(":"))
          throw new BAD_PARAM("':' expected after 'rir'");

        key = readKey("/");

        Object object;
        try
          {
            object = orb.resolve_initial_references(key);
            return resolve(orb.object_to_string(object));
          }
        catch (InvalidName e)
          {
            throw new BAD_PARAM("Unknown initial reference '" + key + "'");
          }
      }
    else
    // Check for iiop.
    if (t[p].equals(IIOP) || t[p].equals(":"))
      {
        IOR ior = new IOR();

        Addresses: do
          { // Read addresses.
            if (t[p].equals(":"))
              {
                p++;
              }
            else
              {
                p++;
                if (!t[p++].equals(":"))
                  throw new BAD_PARAM("':' expected after 'iiop'");
                // Check if version is present.
                if (t[p + 1].equals("."))
                  if (t[p + 3].equals("@"))
                    {
                      // Version info present.
                      try
                        {
                          major = Integer.parseInt(t[p++]);
                        }
                      catch (NumberFormatException e)
                        {
                          throw new BAD_PARAM("Major version number '"
                            + t[p - 1] + "'");
                        }
                      p++; // '.' at this point.
                      try
                        {
                          minor = Integer.parseInt(t[p++]);
                        }
                      catch (NumberFormatException e)
                        {
                          throw new BAD_PARAM("Major version number '"
                            + t[p - 1] + "'");
                        }
                      p++; // '@' at this point.
                    }
              }

            ior.Internet.version = new Version(major, minor);

            // Then host data goes till '/' or ':'.
            CPStringBuilder bhost = new CPStringBuilder(corbaloc.length());
            while (!t[p].equals(":") && !t[p].equals("/") && !t[p].equals(","))
              bhost.append(t[p++]);

            host = bhost.toString();

            ior.Internet.host = host;

            if (t[p].equals(":"))
              {
                // Port specified.
                p++;
                try
                  {
                    port = Integer.parseInt(t[p++]);
                  }
                catch (NumberFormatException e)
                  {
                    throw new BAD_PARAM("Invalid port '" + t[p - 1] + "'");
                  }
              }

            ior.Internet.port = port;

            // Id is not listed.
            ior.Id = "";

            if (t[p].equals(","))
              p++;
            else
              break Addresses;
          }
        while (true);

        key = readKey("/");
        ior.key = key.getBytes();

        return resolve(ior.toStringifiedReference());
      }

    else
      throw new InvalidNameException("Unsupported protocol '" + t[p] +
                                     "' (iiop expected)");
  }
  
  /**
   * Read IOR from the file in the local file system.
   */
  String readFile(String file)
  {
    File f = new File(file);
    if (!f.exists())
      {
        DATA_CONVERSION err = new DATA_CONVERSION(f.getAbsolutePath()
          + " does not exist.");
        err.minor = Minor.Missing_IOR;
      }
    try
      {
        char[] c = new char[(int) f.length()];
        FileReader fr = new FileReader(f);
        fr.read(c);
        fr.close();
        return new String(c).trim();
      }
    catch (IOException ex)
      {
        DATA_CONVERSION d = new DATA_CONVERSION();
        d.initCause(ex);
        d.minor = Minor.Missing_IOR;
        throw (d);
      }
  }
  
  /**
   * Read IOR from the remote URL.
   */
  String readUrl(String url)
  {
    URL u;
    try
      {
        u = new URL(url);
      }
    catch (MalformedURLException mex)
      {
        throw new BAD_PARAM("Malformed URL: '" + url + "'");
      }

    try
      {
        InputStreamReader r = new InputStreamReader(u.openStream());

        CPStringBuilder b = new CPStringBuilder();
        int c;

        while ((c = r.read()) > 0)
          b.append((char) c);

        return b.toString().trim();
      }
    catch (Exception exc)
      {
        DATA_CONVERSION d = new DATA_CONVERSION("Reading " + url + " failed.");
        d.minor = Minor.Missing_IOR;
        throw d;
      }
  }

  private String[] resolve(String nsIor)
  {
    String [] n = new String[2];
    n[0] = nsIor;
    n[1] = readKey("#");
    return n;
  }

  private String readKey(String delimiter)
    throws BAD_PARAM
  {
    if (p < t.length)
      if (!t[p].equals(delimiter))
        {
          if (t[p].equals("#"))
            return DEFAULT_NAME;
          else
            throw new BAD_PARAM("'" + delimiter + "String' expected '" + t[p]
              + "' found");
        }

    CPStringBuilder bKey = new CPStringBuilder();
    p++;

    while (p < t.length && !t[p].equals("#"))
      bKey.append(t[p++]);

    if (bKey.length() == 0)
      return DEFAULT_NAME;

    try
      {
        return URLDecoder.decode(bKey.toString(), "UTF-8");
      }
    catch (UnsupportedEncodingException e)
      {
        throw new Unexpected("URLDecoder does not support UTF-8", e);
      }
  }
}
