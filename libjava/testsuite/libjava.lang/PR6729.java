public class PR6729
{
    static int attr = 0x9;

    public static void main(String [] args) {
        boolean res = ( ( 1 << attr ) & 0x1000 ) != 0 ;
        System.out.println("this should be "+res+": "+isWhite());
    }

    public static boolean isWhite() {
        return ( ( 1 << attr ) & 0x1000 ) != 0 ;
    }
}
