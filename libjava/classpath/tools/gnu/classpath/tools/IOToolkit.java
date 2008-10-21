/* gnu.classpath.tools.IOToolkit
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;

import java.util.Set;

/**
 *  Provides various I/O-related helper methods.
 *
 *  @author Julian Scheid
 */
public class IOToolkit
{
   /**
    *  Prevents instantiation.
    */
   private IOToolkit() {}

   /**
    *  Read all binary data from the given InputStream and write it to
    *  the given OutputStream. This method doesn't close either
    *  stream.
    *
    *  @param in  the stream from which to read data
    *  @param out the stream to which to write data
    */
   public static void copyStream(InputStream in, OutputStream out)
      throws IOException
   {
      byte[] buf = new byte[256];
      int nread;

      while ((nread = in.read(buf)) >= 0) {
         out.write(buf, 0, nread);
      }
   }

   /**
    *  Read all character data from the given Reader and write it to
    *  the given Writer. This method doesn't close either stream.
    *
    *  @param in  the Reader from which to read character data
    *  @param out the Writer to which to write character data
    */
   public static void copyStream(Reader in, Writer out)
      throws IOException 
   {
      char[] buf = new char[256];
      int nread;

      while ((nread = in.read(buf)) >= 0) {
         out.write(buf, 0, nread);
      }
   }

   /**
    *  Recursively copy the contents of the input directory to the
    *  output directory. The output directory is created if it doesn't
    *  exist. If the output directory doesn't exist and can't be
    *  created, an IOException is thrown.
    *
    *  @param sourceDir source directory from which to copy files
    *  @param targetDir target directory to which to copy files
    *  @param recursive if true, recursively copy subdirectoryies
    *  @param excludeDirs if non null, must be a Set of String. Each
    *  element from the set specifies the name of a direct
    *  subdirectory of the source directory which should be excluded
    *  from recursive copying.
    */
   public static void copyDirectory(File sourceDir, File targetDir, 
                                    boolean recursive,
                                    Set excludeDirs) 
      throws IOException 
   {
      if (!targetDir.exists() && !targetDir.mkdirs()) {
         throw new IOException("Cannot create directory " + targetDir);
      }

      File[] sourceFiles = sourceDir.listFiles();
      for (int i=0; i<sourceFiles.length; ++i) {
         if (sourceFiles[i].isDirectory()) {
            if (recursive && (null == excludeDirs 
                              || !excludeDirs.contains(sourceFiles[i].getName()))) {
               File targetSubDir = new File(targetDir, 
                                            sourceFiles[i].getName());
               if (targetSubDir.exists() || targetSubDir.mkdir()) {
                  copyDirectory(sourceFiles[i], targetSubDir, recursive, null);
               }
               else {
                  throw new IOException("Cannot create directory " + targetSubDir);
               }
            }
         }
         else {
            copyFile(sourceFiles[i], new File(targetDir, sourceFiles[i].getName()));
         }
      }
   }

   /**
    *  Copy the contents of the input file to the output file. The
    *  output file's parent directory must exist.
    *
    *  @param sourceFile  specifies the file to copy
    *  @param targetFile  specifies the file to create
    */
   public static void copyFile(File sourceFile, File targetFile) 
      throws IOException 
   {
      InputStream in = new FileInputStream(sourceFile);
      OutputStream out = new FileOutputStream(targetFile);
      int nread;
      byte[] buf = new byte[512];
      while ((nread = in.read(buf)) >= 0) {
         out.write(buf, 0, nread);
      }
      in.close();
      out.close();
   }

   /**
    *  Read the (remaining) contents of the given reader into a char
    *  array. This method doesn't close the reader when it is done.
    *
    *  @param reader the Reader to read characters from
    *  @return an array with the contents of the Reader
    */
   public static char[] readFully(Reader reader)
      throws IOException
   {
      StringWriter writer = new StringWriter();
      final int readBufferSize = 256;
      char[] chunk = new char[readBufferSize];
      int nread;
      while ((nread=reader.read(chunk))>=0) {
	 writer.write(chunk,0,nread);
      }
      StringBuffer buffer = writer.getBuffer();
      char[] result = new char[buffer.length()];
      buffer.getChars(0, buffer.length(), result, 0);
      return result;
   }

   public static String getLineFromFile(File file, int line)
      throws IOException
   {
      FileReader reader = new FileReader(file);
      BufferedReader bufferedReader = new BufferedReader(reader);
      while (line > 1) {
         bufferedReader.readLine();
         -- line;
      }
      String result = bufferedReader.readLine();
      reader.close();
      return result;
   }

   public static String getColumnDisplayLine(int column)
   {
      StringBuffer result = new StringBuffer();
      while (column > 0) {
         result.append(' ');
         --column;
      }
      result.append('^');
      return result.toString();
   }

}
