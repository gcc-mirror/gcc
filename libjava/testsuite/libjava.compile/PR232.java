// Use of a "static final String" as a monitor causes ICE
// in jc1 (20000520)
//
// Program received signal SIGSEGV, Segmentation fault.
// put_decl_node (node=0x0) at ../../../gcc/java/lang.c:413
// 413       if (TREE_CODE (node) == POINTER_TYPE)

class PR232
{
  static final String lock= "LOCK";
  
  void a()
  {   
    synchronized(lock) {}
  }
};
