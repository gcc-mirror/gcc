// File PrivateInnerInterface.java

public class PR224 {
    private interface Inter {}
}


class PrivateInnerInterface_Test extends PR224 {
    void foo() {
        // Implement the interface with an innerclass
        Inter i = new Inter() { } ;
    }
}

// This should fail to compile because Inter is private in the superclass

