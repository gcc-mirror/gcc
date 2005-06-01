import java.io.*;

public class pr21785 implements Serializable
{
  public static void main(String[] args)
  {
    try {
      ByteArrayOutputStream outb = new ByteArrayOutputStream();
      ObjectOutputStream outs = new ObjectOutputStream(outb);
      outs.writeObject(new pr21785());
      byte[] store = outb.toByteArray();

      ByteArrayInputStream inb = new ByteArrayInputStream(store);
      ObjectInputStream ins = new ObjectInputStream(inb);
      ins.readObject();
    }
    catch (Throwable e) {
      throw new Error(e);
    }
  }
}
