;
; LeftBehind.j - contrived test to see how japhar reacts to
;                stuff left on the stack after a method returns.
.class public LeftBehind
.super java/lang/Object

.method public static test()I
   .limit stack 10   ; up to 10 items can be pushed

   ; push some ints.
   bipush 1
   bipush 2
   bipush 3
   bipush 4
   bipush 5
   ; then push some strings.
   ldc "6th item"
   ldc "7th item"

   bipush 5

   ; now push our return value
   bipush 9

   ireturn
.end method

.method public static main([Ljava/lang/String;)V
   .limit stack 3	; up to three items can be pushed

   ; we push a value onto the stack, and
   ; then check to see that only one item (the return
   ; value from the test() method) is on the stack on top
   ; of it.
   bipush 8

   invokestatic LeftBehind/test()I

   pop      ; get rid of the return value
   bipush 8
   isub

   ifeq pass

fail:
   ; push System.out onto the stack
   getstatic java/lang/System/out Ljava/io/PrintStream;

   ldc "FAILED:"

   invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
   bipush 0
   invokestatic java/lang/System/exit(I)V

pass:   
   ; push System.out onto the stack
   getstatic java/lang/System/out Ljava/io/PrintStream;

   ldc "PASSED:"

   invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
   bipush 0
   invokestatic java/lang/System/exit(I)V
.end method
