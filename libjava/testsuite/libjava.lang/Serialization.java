import java.io.*;
import java.util.*;

public class Serialization
{
  public static void main(String[] args) 
    throws IOException, ClassNotFoundException
  {    
    File f = new File("test.ser");
    ObjectOutputStream objStream = 
      new ObjectOutputStream(new FileOutputStream(f));
    objStream.writeInt(8);
    objStream.writeObject(new Integer(99));
    List l = new LinkedList();
    l.add(new Integer(1));
    l.add(new Short((short) 7));
    l.add(new Float(9.95));
    l.add(new Long(-900000000000001l));
    l.add(new Double(-3.14159));
    l.add(new Character('X'));
    l.add(new Byte((byte) 'z'));
    objStream.writeObject(l);
    objStream.close();
    
    ObjectInputStream ois = new ObjectInputStream(new FileInputStream(f));
    System.out.println (ois.readInt());
    System.out.println (ois.readObject());
    System.out.println (ois.readObject());
    ois.close();
    f.delete();
  }
}
