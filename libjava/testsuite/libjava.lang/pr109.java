// PR 109

// Running the test code produces the output "0" instead of the
// expected "01234".

// The break statement exits both for-loops (not just the innermost
// one) if the (single statement) body of the outer for-loop is not
// enclosed in braces. Affects more deeply nested loops in the same
// way.

public class pr109
{
  public static void main (String argv[])
    {
      int i, j;

      for (i = 0; i < 5; i++)
	for (j = 0; j < 2; j++)
	  {
	    if (j == 1)
	      break;
	    System.out.print (i);
	  }
    }
}
