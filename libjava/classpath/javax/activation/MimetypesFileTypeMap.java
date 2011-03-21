/* MimeTypesFileTypeMap.java -- A file type map using mime.types.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package javax.activation;

import gnu.java.lang.CPStringBuilder;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Implementation of FileTypeMap that uses the <tt>mime.types</tt> format.
 * File entries are searched for in the following locations and order:
 * <ol>
 * <li>Programmatically added entries to this instance</li>
 * <li>The file <tt>.mime.types</tt> in the user's home directory</li>
 * <li>The file <i>&lt;java.home&gt;</i><tt>/lib/mime.types</tt></li>
 * <li>The resource <tt>META-INF/mime.types</tt></li>
 * <li>The resource <tt>META-INF/mimetypes.default</tt> in the JAF
 * distribution</li>
 * </ol>
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
public class MimetypesFileTypeMap
  extends FileTypeMap
{

  private static final int PROG = 0;
  private static final int HOME = 1;
  private static final int SYS = 2;
  private static final int JAR = 3;
  private static final int DEF = 4;
  private static final String DEFAULT_MIME_TYPE = "application/octet-stream";
  private static boolean debug = false;
  static
  {
    try
      {
        String d = System.getProperty("javax.activation.debug");
        debug = Boolean.valueOf(d).booleanValue();
      }
    catch (SecurityException e)
      {
      }
  }

  private Map<String,String>[] mimetypes;

  /**
   * Default constructor.
   */
  public MimetypesFileTypeMap()
  {
    init(null);
  }

  /**
   * Constructor specifying a filename.
   * @param mimeTypeFileName the name of the file to read mime.types
   * entries from
   */
  public MimetypesFileTypeMap(String mimeTypeFileName)
    throws IOException
  {
    Reader in = null;
    try
      {
        in = new FileReader(mimeTypeFileName);
        init(in);
      }
    finally
      {
        if (in != null)
          {
            in.close();
          }
      }
  }

  /**
   * Constructor specifying an input stream.
   * @param is the input stream to read mime.types entries from
   */
  public MimetypesFileTypeMap(InputStream is)
  {
    init(new InputStreamReader(is));
  }

  private void init(Reader in)
  {
    mimetypes = new Map[5];
    for (int i = 0; i < mimetypes.length; i++)
      {
        mimetypes[i] = new HashMap<String,String>();
      }
    if (in != null)
      {
        if (debug)
          {
            System.out.println("MimetypesFileTypeMap: load PROG");
          }
        try
          {
            parse(mimetypes[PROG], in);
          }
        catch (IOException e)
          {
          }
      }

    if (debug)
      {
        System.out.println("MimetypesFileTypeMap: load HOME");
      }
    try
      {
        String home = System.getProperty("user.home");
        if (home != null)
          {
            parseFile(mimetypes[HOME], new CPStringBuilder(home)
                      .append(File.separatorChar)
                      .append(".mime.types")
                      .toString());
          }
      }
    catch (SecurityException e)
      {
      }

    if (debug)
      {
        System.out.println("MimetypesFileTypeMap: load SYS");
      }
    try
      {
        parseFile(mimetypes[SYS],
                  new CPStringBuilder(System.getProperty("java.home"))
                  .append(File.separatorChar)
                  .append("lib")
                  .append(File.separatorChar)
                  .append("mime.types")
                  .toString());
      }
    catch (SecurityException e)
      {
      }
    if (debug)
      {
        System.out.println("MimetypesFileTypeMap: load JAR");
      }
    List<URL> systemResources = getSystemResources("META-INF/mime.types");
    int len = systemResources.size();
    if (len > 0)
      {
        for (int i = 0; i < len ; i++)
          {
            Reader urlIn = null;
            URL url = (URL)systemResources.get(i);
            try
              {
                urlIn = new InputStreamReader(url.openStream());
                parse(mimetypes[JAR], urlIn);
              }
            catch (IOException e)
              {
              }
            finally
              {
                if (urlIn != null)
                  {
                    try
                      {
                        urlIn.close();
                      }
                    catch (IOException e)
                      {
                      }
                  }
              }
          }
      }
    else
      {
        parseResource(mimetypes[JAR], "/META-INF/mime.types");
      }

    if (debug)
      {
        System.out.println("MimetypesFileTypeMap: load DEF");
      }
    parseResource(mimetypes[DEF], "/META-INF/mimetypes.default");
  }

  /**
   * Adds entries prorammatically to the registry.
   * @param mime_types a mime.types formatted entries string
   */
  public synchronized void addMimeTypes(String mime_types)
  {
    if (debug)
      {
        System.out.println("MimetypesFileTypeMap: add to PROG");
      }
    try
      {
        parse(mimetypes[PROG], new StringReader(mime_types));
      }
    catch (IOException e)
      {
      }
  }

  /**
   * Returns the MIME content type of the file.
   * This calls <code>getContentType(f.getName())</code>.
   * @param f the file
   */
  public String getContentType(File f)
  {
    return getContentType(f.getName());
  }

  /**
   * Returns the MIME type based on the given filename.
   * If no entry is found, returns "application/octet-stream".
   * @param filename the filename
   */
  public synchronized String getContentType(String filename)
  {
    int di = filename.lastIndexOf('.');
    if (di < 0)
      {
        return DEFAULT_MIME_TYPE;
      }
    String tail = filename.substring(di + 1);
    if (tail.length() < 1)
      {
        return DEFAULT_MIME_TYPE;
      }
    for (int i = 0; i < mimetypes.length; i++)
      {
        String mimeType = (String)mimetypes[i].get(tail);
        if (mimeType != null)
          {
            return mimeType;
          }
      }
    return DEFAULT_MIME_TYPE;
  }

  private void parseFile(Map<String,String> mimetypes, String filename)
  {
    Reader in = null;
    try
      {
        in = new FileReader(filename);
        parse(mimetypes, in);
      }
    catch (IOException e)
      {
      }
    finally
      {
        if (in != null)
          {
            try
              {
                in.close();
              }
            catch (IOException e)
              {
              }
          }
      }
  }

  private void parseResource(Map<String,String> mimetypes, String name)
  {
    Reader in = null;
    try
      {
        InputStream is = getClass().getResourceAsStream(name);
        if (is != null)
          {
            in = new InputStreamReader(is);
            parse(mimetypes, in);
          }
      }
    catch (IOException e)
      {
      }
    finally
      {
        if (in != null)
          {
            try
              {
                in.close();
              }
            catch (IOException e)
              {
              }
          }
      }
  }

  private void parse(Map<String,String> mimetypes, Reader in)
    throws IOException
  {
    BufferedReader br = new BufferedReader(in);
    CPStringBuilder buf = null;
    for (String line = br.readLine(); line != null; line = br.readLine())
      {
        line = line.trim();
        int len = line.length();
        if (len == 0 || line.charAt(0) == '#')
          {
            continue; // Empty line / comment
          }
        if (line.charAt(len - 1) == '\\')
          {
            if (buf == null)
              {
                buf = new CPStringBuilder();
              }
            buf.append(line.substring(0, len - 1));
          }
        else if (buf != null)
          {
            buf.append(line);
            parseEntry(mimetypes, buf.toString());
            buf = null;
          }
        else
          {
            parseEntry(mimetypes, line);
          }
      }
  }

  private void parseEntry(Map<String,String> mimetypes,
                          String line)
  {
    // Tokenize
    String mimeType = null;
    char[] chars = line.toCharArray();
    int len = chars.length;
    CPStringBuilder buffer = new CPStringBuilder();
    for (int i = 0; i < len; i++)
      {
        char c = chars[i];
        if (Character.isWhitespace(c))
          {
            if (mimeType == null)
              {
                mimeType = buffer.toString();
              }
            else if (buffer.length() > 0)
              {
                mimetypes.put(buffer.toString(), mimeType);
              }
            buffer.setLength(0);
          }
        else
          buffer.append(c);
      }
    if (buffer.length() > 0)
      {
        mimetypes.put(buffer.toString(), mimeType);
      }
  }

  // -- Utility methods --

  private List<URL> getSystemResources(String name)
  {
    List<URL> acc = new ArrayList<URL>();
    try
      {
        for (Enumeration<URL> i = ClassLoader.getSystemResources(name);
             i.hasMoreElements(); )
          acc.add(i.nextElement());
      }
    catch (IOException e)
      {
      }
    return acc;
  }

}
