interface A
{
    int a = 0;
}
interface B extends A
{
}
interface C extends A
{
}

public class PR12416 implements B, C
{
    static public void main (String[] unused)
    {
	java.lang.reflect.Field[] fields = PR12416.class.getFields();

	for (int i = 0; i != fields.length; ++i) {
	    System.out.println (fields[i]);
	}
    }
}
