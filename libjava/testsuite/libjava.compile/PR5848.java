import java.util.Vector;
import java.util.Enumeration;

public class PR5848
{
  private Vector data;
  void sub()
  {
    long sz = 0;
    for (Enumeration e = data.elements() ; e.hasMoreElements() ;) {
      sz =+ ((byte[])e.nextElement()).length;
    }
  }
}
