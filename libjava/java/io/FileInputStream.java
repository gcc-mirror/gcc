/* FileInputStream.java -- An input stream that reads from disk files.
   Copyright (C) 1998, 1999, 2001, 2002, 2003 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.io;

import java.nio.channels.FileChannel;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 28, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class FileInputStream extends InputStream
{
  /* Contains the file descriptor for referencing the actual file. */
  private FileDescriptor fd;

  private FileChannel ch;

  public FileInputStream(String name) throws FileNotFoundException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkRead(name);
    fd = new FileDescriptor(name, FileDescriptor.READ);
  }

  public FileInputStream(File file) throws FileNotFoundException
  {
    this(file.getPath());
  }

  public FileInputStream(FileDescriptor fdObj)
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkRead(fdObj);
    fd = fdObj;
  }

  public int available() throws IOException
  {
    return fd.available();
  }

  public void close() throws IOException
  {
    if (fd.valid())
      fd.close();
  }

  protected void finalize() throws IOException
  {
    // We don't actually need this, but we include it because it is
    // mentioned in the JCL.
  }

  public final FileDescriptor getFD() throws IOException
  {
    if (!fd.valid())
      throw new IOException();
    return fd;
  }

  public int read() throws IOException
  {
    return fd.read();
  }

  public int read(byte[] b) throws IOException
  {
    return fd.read(b, 0, b.length);
  }

  public int read(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    return fd.read(b, off, len);
  }

  public long skip(long n) throws IOException
  {
    long startPos = fd.getFilePointer();
    long endPos = fd.seek(n, FileDescriptor.CUR, true);
    return endPos - startPos;
  }

  public FileChannel getChannel ()
  {
    return ch;
  }
}
