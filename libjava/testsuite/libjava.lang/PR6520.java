public class PR6520
{
   public static void check (double x, double y)
   {
     System.out.println (x == y);
   }
 
   public static void main(String[] args)
   {
     check (Math.min (2.0f, Float.NaN), Float.NaN);
   }
 }

