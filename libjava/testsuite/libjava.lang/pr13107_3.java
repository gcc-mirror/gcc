public class pr13107_3
{
  public static void main(String[] args)
  {
    for (int i = 0; i < 1; i++)
      {
	try {
	  System.out.println(i);
	}
	finally {
	  if (i == 3)
	    continue;
	}
      }
  }
}
