/* PasswordFile.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.sasl.plain;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.action.GetPropertyAction;
import gnu.javax.crypto.sasl.NoSuchUserException;
import gnu.javax.crypto.sasl.UserAlreadyExistsException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.security.AccessController;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

/**
 * A representation of a Plain password file.
 */
public class PasswordFile
{
  private static String DEFAULT_FILE;
  static
    {
      DEFAULT_FILE = (String) AccessController.doPrivileged
          (new GetPropertyAction(PlainRegistry.PASSWORD_FILE,
          PlainRegistry.DEFAULT_PASSWORD_FILE));
    }
  private Hashtable entries;
  private File passwdFile;
  private long lastmod;

  public PasswordFile() throws IOException
  {
    this(DEFAULT_FILE);
  }

  public PasswordFile(File pwFile) throws IOException
  {
    this(pwFile.getAbsolutePath());
  }

  public PasswordFile(String fileName) throws IOException
  {
    passwdFile = new File(fileName);
    update();
  }

  public synchronized void add(String user, String passwd, String[] attributes)
      throws IOException
  {
    checkCurrent();
    if (entries.containsKey(user))
      throw new UserAlreadyExistsException(user);
    if (attributes.length != 5)
      throw new IllegalArgumentException("Wrong number of attributes");
    // create the new entry
    String[] fields = new String[7];
    fields[0] = user;
    fields[1] = passwd;
    System.arraycopy(attributes, 0, fields, 2, 5);
    entries.put(user, fields);
    savePasswd();
  }

  public synchronized void changePasswd(String user, String passwd)
      throws IOException
  {
    checkCurrent();
    if (! entries.containsKey(user))
      throw new NoSuchUserException(user);
    String[] fields = (String[]) entries.get(user); // get the existing entry
    fields[1] = passwd; // modify the password field
    entries.remove(user); // delete the existing entry
    entries.put(user, fields); // add the new entry
    savePasswd();
  }

  public synchronized String[] lookup(String user) throws IOException
  {
    checkCurrent();
    if (! entries.containsKey(user))
      throw new NoSuchUserException(user);
    return (String[]) entries.get(user);
  }

  public synchronized boolean contains(String s) throws IOException
  {
    checkCurrent();
    return entries.containsKey(s);
  }

  private synchronized void update() throws IOException
  {
    lastmod = passwdFile.lastModified();
    readPasswd(new FileInputStream(passwdFile));
  }

  private void checkCurrent() throws IOException
  {
    if (passwdFile.lastModified() > lastmod)
      update();
  }

  private synchronized void readPasswd(InputStream in) throws IOException
  {
    BufferedReader din = new BufferedReader(new InputStreamReader(in));
    String line;
    entries = new Hashtable();
    String[] fields = new String[7];
    while ((line = din.readLine()) != null)
      {
        StringTokenizer st = new StringTokenizer(line, ":", true);
        try
          {
            fields[0] = st.nextToken(); // username
            st.nextToken();
            fields[1] = st.nextToken(); // passwd
            if (fields[1].equals(":"))
              fields[1] = "";
            else
              st.nextToken();
            fields[2] = st.nextToken(); // uid
            if (fields[2].equals(":"))
              fields[2] = "";
            else
              st.nextToken();
            fields[3] = st.nextToken(); // gid
            if (fields[3].equals(":"))
              fields[3] = "";
            else
              st.nextToken();
            fields[4] = st.nextToken(); // gecos
            if (fields[4].equals(":"))
              fields[4] = "";
            else
              st.nextToken();
            fields[5] = st.nextToken(); // dir
            if (fields[5].equals(":"))
              fields[5] = "";
            else
              st.nextToken();
            fields[6] = st.nextToken(); // shell
            if (fields[6].equals(":"))
              fields[6] = "";
          }
        catch (NoSuchElementException ignored)
          {
            continue;
          }
        entries.put(fields[0], fields);
      }
  }

  private synchronized void savePasswd() throws IOException
  {
    if (passwdFile != null)
      {
        FileOutputStream fos = new FileOutputStream(passwdFile);
        PrintWriter pw = null;
        try
          {
            pw = new PrintWriter(fos);
            String key;
            String[] fields;
            CPStringBuilder sb;
            Enumeration keys = entries.keys();
            while (keys.hasMoreElements())
              {
                key = (String) keys.nextElement();
                fields = (String[]) entries.get(key);
                sb = new CPStringBuilder(fields[0]);
                for (int i = 1; i < fields.length; i++)
                  sb.append(":" + fields[i]);
                pw.println(sb.toString());
              }
          }
        finally
          {
            if (pw != null)
              try
                {
                  pw.flush();
                }
              finally
                {
                  pw.close();
                }
            if (fos != null)
              try
                {
                  fos.close();
                }
              catch (IOException ignored)
                {
                }
            lastmod = passwdFile.lastModified();
          }
      }
  }
}
