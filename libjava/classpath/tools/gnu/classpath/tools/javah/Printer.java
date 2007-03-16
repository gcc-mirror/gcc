/* Print.java - abstract base class for printing classes
 Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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


package gnu.classpath.tools.javah;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

public abstract class Printer
{
  protected Main classpath;

  /**
   * The {@link File} object that denotes either a directory (when the
   * <code>-d</code> option was used), or a file (when the <code>-o</code>
   * option was used) on the command line.
   */
  protected File outputFileObject;

  /**
   * Set to <code>true</code> if the field <code>outputFileObject</code> denotes
   * a directory; i.e. for each input class file, one JNI header file will be
   * generated in that directory.
   * <p>
   * Set to <code>false</code> if the field <code>outputFileObject</code>
   * denotes a file; i.e. all generated headers will be written to that file.
   */
  protected boolean isDirectory;

  /**
   * Set to <code>true</code> if the output file(s) should always be written.
   * <p>
   * When set to <code>false</code>, the contents of the header/stub are only
   * written to the file if it does not already exist.
   */
  protected boolean force;

  /**
   * Set to <code>true</code> if all output is directed to one file, and the
   * common preamble text has already been generated.
   */
  protected boolean wrotePreamble;

  protected Printer(Main classpath, File outFile, boolean isDir, boolean force)
  {
    this.classpath = classpath;
    if (outFile == null)
      throw new IllegalArgumentException("File argument MUST NOT be null");
    outputFileObject = outFile;
    isDirectory = isDir;
    if (! isDirectory)
      {
        File parent = outputFileObject.getParentFile();
        if (parent != null)
          parent.mkdirs();
      }
    this.force = force;
  }

  public abstract void printClass(File filename, ClassWrapper klass)
    throws IOException;

  protected abstract void writePreambleImpl(PrintStream ps);

  protected abstract PrintStream getPrintStreamImpl(FileOutputStream fos,
                                                    ClassWrapper klass);

  protected PrintStream getPrintStream(String fullName, ClassWrapper klass)
      throws FileNotFoundException
  {
    PrintStream result;
    FileOutputStream fos;
    if (isDirectory)
      {
        File outFile = new File(outputFileObject, fullName);
        if (outFile.exists() && ! force)
          return null;
        File parent = outFile.getParentFile();
        if (parent != null)
          parent.mkdirs();
        fos = new FileOutputStream(outFile);
        result = getPrintStreamImpl(fos, klass);
        writePreamble(result);
      }
    else
      {
        // the first time we open this file, wrotePreamble is false
        fos = new FileOutputStream(outputFileObject, wrotePreamble);
        result = getPrintStreamImpl(fos, klass);
        if (! wrotePreamble)
          writePreamble(result);
      }
    return result;
  }

  protected void writePreamble(PrintStream out)
  {
    writePreambleImpl(out);
    wrotePreamble = true;
  }
}
