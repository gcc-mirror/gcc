public class emptystring
{
        public static void main(String[] args)
        {
                System.out.println("null".equals(n(0) + ""));
                System.out.println("null".equals("" + n(0)));
                System.out.println("x".equals(n(1) + ""));
                System.out.println("x".equals("" + n(1)));
        }

        static String n(int i)
        {
                if (i==0) return null; else return "x";
        }
}
