import java.util.Arrays;
import java.util.Comparator;

public class arraysort
{
  private static final Comparator<String> STRING_COMPARATOR = new Comparator<String>()
  {
    public int compare(String str1, String str2)
    {
      return str1.compareTo(str2);
    }
  };

  static void dumpArray(String[] strings)
  {
    int i;

    for (i = 0 ; i < strings.length ; i++)
    {
      System.out.println("[" + i + "] " + strings[i]);
    }
  }

  public static void main(String[] args)
  {
    String[] strings;

    strings = new String[4];

    strings[0] = "a";
    strings[1] = "c";
    strings[2] = "b";
    strings[3] = "d";

    System.out.println("Array of string, before:");
    dumpArray(strings);

    Arrays.sort(strings, STRING_COMPARATOR);

    System.out.println("Array of string, after:");
    dumpArray(strings);
  }
}

