module structalign;

void main ()
{
    struct K { int *a; }
    K k;
    auto ti = typeid (k);

    assert (ti.flags () == 1);

    auto ti2 = typeid (k.a);
    assert (ti.talign () == ti2.talign ());
}
