using System.Diagnostics;
using OffiUtils;

var allocated = StringUtils.FastAllocate(32);

Debugger.Break();