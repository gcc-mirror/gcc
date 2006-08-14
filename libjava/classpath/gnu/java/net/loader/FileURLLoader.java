/* FileURLLoader.java -- a URLLoader for file URLs
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


package gnu.java.net.loader;


import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLStreamHandlerFactory;
import java.util.StringTokenizer;

/**
 * A <code>FileURLLoader</code> is a type of <code>URLLoader</code>
 * only loading from file url.
 */
public final class FileURLLoader extends URLLoader
{
  File dir; //the file for this file url

  public FileURLLoader(URLClassLoader classloader,
                       URLStreamHandlerCache cache,
                       URLStreamHandlerFactory factory,
                       URL url, URL absoluteUrl)
  {
    super(classloader, cache, factory, url, absoluteUrl);
    dir = new File(absoluteUrl.getFile());
  }

  /** get resource with the name "name" in the file url */
  public Resource getResource(String name)
  {
    try 
      {
        // Make sure that all components in name are valid by walking through
        // them
        File file = walkPathComponents(name);

        if (file == null)
          return null;

        return new FileResource(this, file);
      }
    catch (IOException e)
      {
        // Fall through...
      }
    return null;
  }

  /**
   * Walk all path tokens and check them for validity. At no moment, we are
   * allowed to reach a directory located "above" the root directory, stored
   * in "dir" property. We are also not allowed to enter a non existing
   * directory or a non directory component (plain file, symbolic link, ...).
   * An empty or null path is valid. Pathnames components are separated by
   * <code>File.separatorChar</code>
   * 
   * @param resourceFileName the name to be checked for validity.
   * @return the canonical file pointed by the resourceFileName or null if the
   *         walking failed
   * @throws IOException in case of issue when creating the canonical
   *           resulting file
   * @see File#separatorChar
   */
  private File walkPathComponents(String resourceFileName) throws IOException
  {
    StringTokenizer stringTokenizer = new StringTokenizer(resourceFileName, File.separator);
    File currentFile = dir;
    int tokenCount = stringTokenizer.countTokens();

    for (int i = 0; i < tokenCount - 1; i++)
      {
        String currentToken = stringTokenizer.nextToken();
        
        // If we are at the root directory and trying to go up, the walking is
        // finished with an error
        if ("..".equals(currentToken) && currentFile.equals(dir))
          return null;
        
        currentFile = new File(currentFile, currentToken);

        // If the current file doesn't exist or is not a directory, the walking is
        // finished with an error
        if (! (currentFile.exists() && currentFile.isDirectory()))
          return null;
        
      }
    
    // Treat the last token differently, if it exists, because it does not need
    // to be a directory
    if (tokenCount > 0)
      {
        String currentToken = stringTokenizer.nextToken();
        
        if ("..".equals(currentToken) && currentFile.equals(dir))
          return null;
        
        currentFile = new File(currentFile, currentToken);

        // If the current file doesn't exist, the walking is
        // finished with an error
        if (! currentFile.exists())
          return null;
    }
    
    return currentFile.getCanonicalFile();
  }
}