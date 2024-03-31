namespace OffiUtils;

internal delegate string FastAllocateStringDelegate(int length);
internal delegate ref char GetRawStringDataDelegate(string instance);
internal delegate bool TryUnescapeDelegate(ReadOnlySpan<byte> source, Span<byte> destination, out int written);
