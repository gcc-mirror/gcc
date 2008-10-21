/* gnu.classpath.tools.FileSystemClassLoader
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA. */

package gnu.classpath.tools;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;
import java.util.StringTokenizer;

import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.jar.Attributes;

/**
 *  A <code>ClassLoader</code> implementation which looks for classes
 *  on the local filesystem given a standard search path.
 */
public class FileSystemClassLoader extends ClassLoader {

   private File[] pathComponents;

   /**
    *  Initialize the class loader with a normal path string. The path
    *  string should contain path components separated by {@link
    *  File.pathSeparator}. Each path component should either denote a
    *  directory or a .jar or .zip file.
    */
   public FileSystemClassLoader(String path)
   {
      List components = new ArrayList();
      for (StringTokenizer st = new StringTokenizer(path, File.pathSeparator); st.hasMoreTokens(); ) {
         File pathComponent = new File(st.nextToken());
         components.add(pathComponent);
         if (pathComponent.exists() && !pathComponent.isDirectory()) {
            List subComponents = tryGetJarFileClassPathComponents(pathComponent);
            if (null != subComponents) {
               components.addAll(subComponents);
            }
         }
      }
      File[] componentArray = new File[components.size()];
      this.pathComponents = (File[])components.toArray(componentArray);
   }

   /**
    *  Initialize the class loader with an array of path
    *  components. Each path component should either denote a
    *  directory or a .jar or .zip file.
    */
   public FileSystemClassLoader(File[] pathComponents)
   {
      this.pathComponents = pathComponents;
      for (int i = 0; i < pathComponents.length; ++i) {
         if (!pathComponents[i].exists()) {
            System.err.println("WARNING: Path component '" + pathComponents[i] + "' not found.");
         }
      }
   }

   public Class loadClass(String name)
      throws ClassNotFoundException {

      return super.loadClass(name);
   }

   public Class findClass(String name)
      throws ClassNotFoundException {

      byte[] b = loadClassData(name);
      return defineClass(name, b, 0, b.length);
   }

   public URL findResource(String name)
   {
      StreamInfo streamInfo = getResourceStream(name);
      if (null == streamInfo) {
         return super.findResource(name);
      }
      else {
         try {
            return streamInfo.getURL();
         }
         catch (MalformedURLException e) {
            System.err.println("WARNING: In FileSystemClassLoader: could not derive URL from file or jar entry: " + e.toString());
            return null;
         }
      }
   }

   private byte[] readFromStream(InputStream in, long size) 
      throws IOException
   {
      byte[] result = new byte[(int)size];
      int nread = 0;
      int offset = 0;
      while (offset < size && (nread = in.read(result, offset, (int)(size - offset))) >= 0) {
         offset += nread;
      }
      in.close();
      return result;
   }

   private byte[] readFromStream(StreamInfo streamInfo)
      throws IOException
   {
      InputStream in = streamInfo.openStream();
      long size = streamInfo.getSize();

      byte[] result = new byte[(int)size];
      int nread = 0;
      int offset = 0;
      while (offset < size && (nread = in.read(result, offset, (int)(size - offset))) >= 0) {
         offset += nread;
      }
      in.close();
      return result;
   }

   private static interface StreamInfo
   {
      public InputStream openStream()
         throws IOException;
      public long getSize();
      public URL getURL()
         throws MalformedURLException;
   }

   private static class FileStreamInfo
      implements StreamInfo
   {
      File file;

      FileStreamInfo(File file)
      {
         this.file = file;
      }

      public InputStream openStream()
         throws IOException
      {
         return new FileInputStream(file);
      }

      public long getSize()
      {
         return file.length();
      }

      public URL getURL()
         throws MalformedURLException
      {
         return file.toURL();
      }
   }

   private static class JarStreamInfo
      implements StreamInfo
   {
      private File file;
      private JarFile jarFile;
      private JarEntry jarEntry;

      JarStreamInfo(File file, JarFile jarFile, JarEntry jarEntry)
      {
         this.file = file;
         this.jarFile = jarFile;
         this.jarEntry = jarEntry;
      }

      public InputStream openStream()
         throws IOException
      {
         return jarFile.getInputStream(jarEntry);
      }

      public long getSize()
      {
         return jarEntry.getSize();
      }

      public URL getURL()
         throws MalformedURLException
      {
         String urlString = "jar:" + file.toURL() + "!/" + jarEntry.getName();
         return new URL(urlString);
      }
   }
   
   private StreamInfo getResourceStream(String path)
   {
      for (int i = 0; i < pathComponents.length; ++i) {
         try {
            File parent = pathComponents[i];
            if (parent.isDirectory()) {
               File file = new File(parent, path);
               if (file.exists()) {
                  return new FileStreamInfo(file);
               }
            }
            else {
               JarFile jarFile = new JarFile(parent, false, JarFile.OPEN_READ);
               JarEntry jarEntry = jarFile.getJarEntry(path);
               if (null != jarEntry) {
                  return new JarStreamInfo(parent, jarFile, jarEntry);
               }
            }
         }
         catch (IOException ignore) {
         }
      }
      return null;
   }

   private byte[] loadClassData(String className) 
      throws ClassNotFoundException 
   {
      String classFileName = className.replace('.', File.separatorChar) + ".class";
      StreamInfo streamInfo = getResourceStream(classFileName);

      try {
         if (null != streamInfo) {
            return readFromStream(streamInfo);
         }
      }
      catch (IOException ignore) {
      }

      throw new ClassNotFoundException(className);
   }

   private static List tryGetJarFileClassPathComponents(File file)
   {
      try {
         JarFile jarFile = new JarFile(file, false, JarFile.OPEN_READ);
         Manifest manifest = jarFile.getManifest();
         if (null != manifest) {
            Attributes mainAttributes = manifest.getMainAttributes();
            if (null != mainAttributes) {
               String classPath = mainAttributes.getValue(Attributes.Name.CLASS_PATH);
               if (null != classPath) {
                  List result = new LinkedList();
                  StreamTokenizer tokenizer = new StreamTokenizer(new StringReader(classPath));
                  tokenizer.resetSyntax();
                  tokenizer.wordChars(0, Integer.MAX_VALUE);
                  tokenizer.whitespaceChars(9, 9);   // tab
                  tokenizer.whitespaceChars(10, 10); // lf
                  tokenizer.whitespaceChars(13, 13); // cr
                  tokenizer.whitespaceChars(32, 32); // space
                  tokenizer.quoteChar('"');
                  int token;
                  while ((token = tokenizer.nextToken()) != StreamTokenizer.TT_EOF) {
                     if (StreamTokenizer.TT_WORD == token) {
                        result.add(new File(file.getParentFile(), tokenizer.sval));
                     }
                  }
                  return result;
               }
            }
         }
      }
      catch (IOException ignore) {
      }
      return null;
   }
}

