module std.digest.digest;

import _newDigest = std.digest;

// scheduled for deprecation in 2.077
// See also: https://github.com/dlang/phobos/pull/5013#issuecomment-313987845
alias isDigest = _newDigest.isDigest;
alias DigestType = _newDigest.DigestType;
alias hasPeek = _newDigest.hasPeek;
alias hasBlockSize = _newDigest.hasBlockSize;
alias digest = _newDigest.digest;
alias hexDigest = _newDigest.hexDigest;
alias makeDigest = _newDigest.makeDigest;
alias Digest = _newDigest.Digest;
alias Order = _newDigest.Order;
alias toHexString = _newDigest.toHexString;
alias asArray = _newDigest.asArray;
alias digestLength = _newDigest.digestLength;
alias WrapperDigest = _newDigest.WrapperDigest;
alias secureEqual = _newDigest.secureEqual;
alias LetterCase = _newDigest.LetterCase;
