/* Test to ensure files >= 2^31 bytes are supported. */

import java.io.*;

public class LargeFile
{
  public static void main(String[] args) throws IOException
  {
    File file = new File("LargeFile.tmp");

    try
      {
	RandomAccessFile rfile = new RandomAccessFile(file, "rw");

	long pos = (long) Math.pow(2, 31);

	rfile.seek(pos);
	rfile.write('O');
	rfile.write('K');
	rfile.close();

	// Re-open, read byte back using FileInputStream and clean up.

	FileInputStream fis = new FileInputStream(file);
	fis.skip(pos);
	System.out.print((char) fis.read());
	System.out.println((char) fis.read());
	fis.close();
      }
    finally
      {
        if (file.exists())
	  file.delete();
      }
  }
}
