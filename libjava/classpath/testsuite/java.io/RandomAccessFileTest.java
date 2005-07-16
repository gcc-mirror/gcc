
import java.io.*;

public class RandomAccessFileTest {
  public static void main (String args[]) {
    try {
      File f = new File("/etc/passwd");
      RandomAccessFile rf = new RandomAccessFile(f,"r");

      long length = rf.length();

      rf.seek(length - 10);
      long pos = rf.getFilePointer();

      if ( (length - 10) != pos )
	throw new Exception("Bad value from getFilePointer(), " +
			    pos + " !=" + (length - 10));

      int i = rf.read();
      byte b = rf.readByte();
      boolean test = rf.readBoolean();

      byte buf[] = new byte[40];
      rf.seek(0);
      rf.read(buf);

      rf.close();
      try {
	length = rf.length();
	throw new Exception("Got length from closed RandomAccessFile().");
      } catch (IOException e) {}

      String filename2 = "/var/tmp/testfile-remove";

      File f2 = new File(filename2);
      RandomAccessFile rf2 = new RandomAccessFile(filename2, "rw");

      rf2.write(100);
      rf2.write(buf);

      rf2.close();
      f2.delete();

      System.out.println("PASSED: RandomAccessFile worked.");
      System.exit(0);
    } catch (Exception e) {
      System.out.println("FAILED: "+e);
    }
  }
}
