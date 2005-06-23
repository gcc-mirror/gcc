public class PR20697
{
    public interface I
    {
        public void m();
    }

    public static class A2 implements I
    {
        public void m()
        {
            return;
        }
    }
    
}

class Test extends PR20697.A2
{
    public void m()
    {
        return;
    }

    public void n()
    {
        m();
    }
}
