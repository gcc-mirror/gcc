public class PR208
{
  public String toString () 
  {
    StringBuffer sb = new StringBuffer("");
    
    sb.append (new java.util.Date().getTime() / 1000);
    
    try {
    }
    catch (java.io.IOException e) {
    }

    return sb.toString();
  }
}
