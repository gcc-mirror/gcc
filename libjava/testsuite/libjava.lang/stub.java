// Class stub
// Generated on Fri Feb  4 20:23:47 PST 2000
// A somewhat thorough test of function invocator generated stubs.

class stub {

    String ok;

    void OK () {System.out.println (ok);}
    void OK (int i) {System.out.println (ok+" "+i);}
    static void testOK () {System.out.println ("OK");}
    static void testOK (int i) {System.out.println ("OK "+i); }

    // STATIC   PRIVATE         R_VALUE         ARGS
    // No       No              No              No
                                void    t1      () {OK();}
    // Yes      No              No              No
       static                   void    t2      () {testOK();}
    // No       Yes             No              No
                private         void    t3      () {OK();}
    // Yes      Yes             No              No
       static   private         void    t4      () {testOK();}
    // No       No              Yes             No
                                int     t5      () {return 5;}
    // Yes      No              Yes             No
       static                   int     t6      () {return 6;}
    // No       Yes             Yes             No
                private         int     t7      () {return 7;}
    // Yes      Yes             Yes             No
       static   private         int     t8      () {return 8;}

    // No       No              No              Yes
                                void    t9      (int i) {OK(i);}
    // Yes      No              No              Yes
       static                   void    t10     (int i) {testOK(i);}
    // No       Yes             No              Yes
                private         void    t11     (int i) {OK(i);}
    // Yes      Yes             No              Yes
       static   private         void    t12     (int i) {testOK(i);}
    // No       No              Yes             Yes
                                int     t13     (int i) {return i*2;}
    // Yes      No              Yes             Yes
       static                   int     t14     (int i) {return i*3;}
    // No       Yes             Yes             Yes
                private         int     t15     (int i) {return i*4;}
    // Yes      Yes             Yes             Yes
       static   private         int     t16     (int i) {return i*5;}

    void foo ()
    {
        this.new bar ().test ();
    }
    class bar {
        void test () {
            ok = "OK";
            t1 ();
            t2 ();
            t3 ();
            t4 ();
            System.out.println (t5());
            System.out.println (t6());
            System.out.println (t7());
            System.out.println (t8());
            t9 (9);
            t10 (10);
            t11 (11);
            t12 (12);
            System.out.println (t13(13));
            System.out.println (t14(14));
            System.out.println (t15(15));
            System.out.println (t16(16));
            this.new baz ().test ();
        }
        class baz {
            void test () {
                ok = "OKOK";
                t1 ();
                t2 ();
                t3 ();
                t4 ();
                System.out.println (t5());
                System.out.println (t6());
                System.out.println (t7());
                System.out.println (t8());
                t9 (9);
                t10 (10);
                t11 (11);
                t12 (12);
                System.out.println (t13(13));
                System.out.println (t14(14));
                System.out.println (t15(15));
                System.out.println (t16(16));
            }
        }
    }
    public static void main (String[] arg)
    {   
        System.out.println ("Testing class `stub'...");
        new stub ().foo ();
    }
}
