// PR java/16927
public class AssertBug {
    public void bug(Integer i) {
        assert(false):
            i.toString() + "!";
    }
}
