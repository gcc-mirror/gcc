/* gnu.classpath.tools.doclets.xmldoclet.doctranslet.JarClassLoader
   Copyright (C) 2001 Free Software Foundation, Inc.

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

package gnu.classpath.tools.doclets.xmldoclet.doctranslet;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import java.util.jar.JarFile;
import java.util.jar.JarEntry;

public class JarClassLoader extends ClassLoader {

   private JarFile jarFile;

   public JarClassLoader(JarFile jarFile) {
      this.jarFile = jarFile;
   }

   public Class findClass(String name)
      throws ClassNotFoundException {

      byte[] b = loadClassData(name);
      return defineClass(name, b, 0, b.length);
   }

   private byte[] loadClassData(String className) 
      throws ClassNotFoundException 
   {
      String classFileName = className.replace('.', File.separatorChar) + ".class";

      try {
         JarEntry jarEntry = jarFile.getJarEntry(classFileName);
         if (null != jarEntry) {
            return readFromStream(jarFile.getInputStream(jarEntry), 
                                  jarEntry.getSize());
         }
      }
      catch (IOException ignore_) {
      }
      throw new ClassNotFoundException(className);
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
}
