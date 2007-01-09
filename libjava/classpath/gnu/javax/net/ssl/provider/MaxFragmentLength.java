package gnu.javax.net.ssl.provider;

import gnu.javax.net.ssl.provider.Extension.Value;

import java.nio.ByteBuffer;

/**
 * Extension value 
 * @author csm
 */
public class MaxFragmentLength extends Value
{
  public static final MaxFragmentLength LEN_2_9 = new MaxFragmentLength(1, 1 << 9);
  public static final MaxFragmentLength LEN_2_10 = new MaxFragmentLength(2, 1 << 10);
  public static final MaxFragmentLength LEN_2_11 = new MaxFragmentLength(3, 1 << 11);
  public static final MaxFragmentLength LEN_2_12 = new MaxFragmentLength(4, 1 << 12);
  
  private final int value;
  private final int length;
  
  private MaxFragmentLength(int value, int length)
  {
    this.value = value;
    this.length = length;
  }
  
  public ByteBuffer buffer()
  {
    return ByteBuffer.allocate(1).put(0, (byte) value);
  }
  
  public int length()
  {
    return 1;
  }

  public int getValue()
  {
    return value;
  }
  
  public int maxLength()
  {
    return length;
  }
  
  public String toString()
  {
    return toString(null);
  }
  
  public String toString(String prefix)
  {
    String s = "max_fragment_length = ";
    if (prefix != null)
      s = prefix + s;
    return s + maxLength() + ";";
  }
}
