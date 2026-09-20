# Rustc lib patches

This directory contains backported patches that are required for Rust for Linux to compile. This is because our current target version for `core` is 1.49.0, while Rust for Linux requires 1.85.0 (since Linux kernel 7.1).

It should be noted that the patches are intentionally kept separate from the rustc-lib directory that contains version 1.49 of the `core` library. This is because gccrs is also not only aiming to compile `core` 1.49, but also currently experimenting with compiling Rust for Linux. **Only use the patches in this directory if you are compiling Rust for Linux with gccrs.**

## Usage

Run `rustc-lib-patch.py` to apply the patches to the `rustc-lib` directory in your local GCC repository.

If you want to do it outside of the tree, specify `-o` to output the patched library directory to a different location without affecting the original `rustc-lib` directory. For example:

```bash
python3 rustc-lib-patch.py -o /path/to/output/rustc-lib
```

## Contributing new patches

1. First, apply all the existing patches to your local `rustc-lib` directory with the python script. Make sure that the library is in a clean state (no uncommitted changes) before applying the patches.
2. Make the necessary changes to the library and run `rustc-lib-patch.py --generate 0003-my-new-patch.for-rfl-patch` to generate a new patch file.
3. Submit a pull request with the new patch file added to `patches/`.

The more advanced git users can just run `git diff` to generate a patch, but the script ensures that the patch file is generated in a way that is compatible with the existing patches.
