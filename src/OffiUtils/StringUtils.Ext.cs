using System.Runtime.InteropServices;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public ref char GetRawData() => ref MemoryMarshal.GetReference(value.AsSpan());
        public Span<char> GetSpan() => MemoryMarshal.CreateSpan(ref value.GetRawData(), value.Length);
    }

    extension(ReadOnlySpan<char> value)
    {
        public string ToPooledString(StringPool? pool = null)
            => (pool ?? StringPool.Shared).GetOrAdd(value);
    }
}
