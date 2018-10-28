/**
 * Treap container for internal usage.
 *
 * Copyright: Copyright Digital Mars 2014 - 2014.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 */
module rt.util.container.treap;

static import common = rt.util.container.common;
import rt.util.random;
import rt.qsort;

struct Treap(E)
{
nothrow:
    static struct Node
    {
        Node* left, right;
        E element;
        uint priority;
    }

    @disable this(this);

    ~this()
    {
        removeAll();
    }

    void initialize()
    {
        rand48.defaultSeed();
    }

    void insert(E element) @nogc
    {
        root = insert(root, element);
    }

    void remove(E element)
    {
        remove(&root, element);
    }

    int opApply(scope int delegate(ref E) nothrow dg)
    {
        return (cast(const)&this).opApply((ref const E e) => dg(*cast(E*)&e));
    }

    int opApply(scope int delegate(ref const E) nothrow dg) const
    {
        return opApplyHelper(root, dg);
    }

    version (unittest)
    bool opEquals(E[] elements)
    {
        size_t i;
        foreach (e; this)
        {
            if (i >= elements.length)
                return false;
            if (e != elements[i++])
                return false;
        }
        return i == elements.length;
    }

    void removeAll()
    {
        removeAll(root);
        root = null;
    }

    version (unittest)
    bool valid()
    {
        return valid(root);
    }


    version (none)
    uint height()
    {
        static uint height(Node* node)
        {
            if (!node)
                return 0;
            auto left = height(node.left);
            auto right = height(node.right);
            return 1 + (left > right ? left : right);
        }
        return height(root);
    }

    version (none)
    size_t count()
    {
        static size_t count(Node* node)
        {
            if (!node)
                return 0;
            return count(node.left) + count(node.right) + 1;
        }
        return count(root);
    }


private:
    Node* root;
    Rand48 rand48;

    Node* allocNode(E element) @nogc
    {
        Node* node = cast(Node*)common.xmalloc(Node.sizeof);
        node.left = node.right = null;
        node.priority = rand48();
        node.element = element;
        return node;
    }

    Node* insert(Node* node, E element) @nogc
    {
        if (!node)
            return allocNode(element);
        else if (element < node.element)
        {
            node.left = insert(node.left, element);
            if (node.left.priority < node.priority)
                node = rotateR(node);
        }
        else if (element > node.element)
        {
            node.right = insert(node.right, element);
            if (node.right.priority < node.priority)
                node = rotateL(node);
        }
        else
        {} // ignore duplicate

        return node;
    }

static:

    void freeNode(Node* node)
    {
        common.free(node);
    }

    Node* rotateL(Node* root)
    {
        auto pivot = root.right;
        root.right = pivot.left;
        pivot.left = root;
        return pivot;
    }

    Node* rotateR(Node* root)
    {
        auto pivot = root.left;
        root.left = pivot.right;
        pivot.right = root;
        return pivot;
    }

    void remove(Node** ppnode, E element)
    {
        Node* node = *ppnode;
        if (!node)
            return; // element not in treap

        if (element < node.element)
        {
            remove(&node.left, element);
        }
        else if (element > node.element)
        {
            remove(&node.right, element);
        }
        else
        {
            while (node.left && node.right)
            {
                if (node.left.priority < node.right.priority)
                {
                    *ppnode = rotateR(node);
                    ppnode = &(*ppnode).right;
                }
                else
                {
                    *ppnode = rotateL(node);
                    ppnode = &(*ppnode).left;
                }
            }
            if (!node.left)
                *ppnode = node.right;
            else
                *ppnode = node.left;
            freeNode(node);
        }
    }

    void removeAll(Node* node)
    {
        if (!node)
            return;
        removeAll(node.left);
        removeAll(node.right);
        freeNode(node);
    }

    int opApplyHelper(const Node* node, scope int delegate(ref const E) nothrow dg)
    {
        if (!node)
            return 0;

        int result = opApplyHelper(node.left, dg);
        if (result)
            return result;
        result = dg(node.element);
        if (result)
            return result;
        return opApplyHelper(node.right, dg);
    }

    version (unittest)
    bool valid(Node* node)
    {
        if (!node)
            return true;

        if (node.left)
        {
            if (node.left.priority < node.priority)
                return false;
            if (node.left.element > node.element)
                return false;
        }
        if (node.right)
        {
            if (node.right.priority < node.priority)
                return false;
            if (node.right.element < node.element)
                return false;
        }
        return valid(node.left) && valid(node.right);
    }
}

unittest
{
    // randomized unittest for randomized data structure
    import /*cstdlib = */core.stdc.stdlib : rand, srand;
    import /*ctime = */core.stdc.time : time;

    enum OP { add, remove }
    enum initialSize = 1000;
    enum randOps = 1000;

    Treap!uint treap;
    OP[] ops;
    uint[] opdata;

    treap.initialize();
    srand(cast(uint)time(null));

    uint[] data;
initialLoop:
    foreach (i; 0 .. initialSize)
    {
        data ~= rand();
        treap.insert(data[$-1]);
        foreach (e; data[0..$-1])
            if (e == data[$-1])
            {
                data = data[0..$-1];
                continue initialLoop;
            }
    }
    _adSort(*cast(void[]*)&data, typeid(data[0]));
    assert(treap == data);
    assert(treap.valid());

    for (int i = randOps; i > 0; --i)
    {
        ops ~= cast(OP)(rand() < uint.max / 2 ? OP.add: OP.remove);
        opdata ~= rand();
    }

    foreach (op; ops)
    {
        if (op == OP.add)
        {
            treap.insert(opdata[0]);

            size_t i;
            for (i = 0; i < data.length; ++i)
                if (data[i] >= opdata[0])
                    break;

            if (i == data.length || data[i] != opdata[0])
            {    // not a duplicate
                data.length++;
                uint tmp = opdata[0];
                for (; i < data.length; ++i)
                {
                    uint tmp2 = data[i];
                    data[i] = tmp;
                    tmp = tmp2;
                }
            }
        }
        else if (!data.length)    // nothing to remove
        {
            opdata = opdata[1..$];
            continue;
        }
        else
        {
            uint tmp = data[opdata[0]%data.length];
            treap.remove(tmp);
            size_t i;
            for (i = 0; data[i] < tmp; ++i)
            {}
            for (; i < data.length-1; ++i)
                data[i] = data[i+1];
            data.length--;
        }
        assert(treap.valid());
        assert(treap == data);
        opdata = opdata[1..$];
    }

    treap.removeAll();
    data.length = 0;
    assert(treap == data);
}
