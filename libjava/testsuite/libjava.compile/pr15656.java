// This used to cause a gcj crash in error_if_numeric_overflow.

public class pr15656 {
        public static void defineClass ()
        {
                Object ctor = new Object;
        }
}
