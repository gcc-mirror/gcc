public class PR21045
{
	class InnerBase {
		InnerBase() throws Exception, NullPointerException {}
	}
	void method() {
		try {
			InnerBase obj = new InnerBase() {};
		} catch (Exception e) {}
	}
}
