// Class final_static_and_friend
// Generated on Sat Feb 12 01:58:38 PST 2000

class final_static_and_friend {
    final int a = 34;
    static int b = 34;
    int c = 34;
    void foo ()
    {
        System.out.println (a+b+c);
    }
    public static void main (String[] arg)
    {
        new final_static_and_friend ().foo ();
    }

}
