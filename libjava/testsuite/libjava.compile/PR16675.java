public class PR16675 {
    public PR16675(Object obj) { }

    public void someTestMethod() {
        // gcj crashed compiling this, as `null' had type `void*'.
        new PR16675(null) { };
    }

    public void someTestMethod2() {
        new PR16675((Object) null) { };
    }

}
