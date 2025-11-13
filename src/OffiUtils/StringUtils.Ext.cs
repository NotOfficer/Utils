using System.Runtime.InteropServices;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    public static ref char GetRawData(this string value) => ref MemoryMarshal.GetReference(value.AsSpan());

    public static Span<char> GetSpan(this string value) => MemoryMarshal.CreateSpan(ref GetRawData(value), value.Length);

    public static string ToPooledString(this ReadOnlySpan<char> value, StringPool? pool = null)
        => (pool ?? StringPool.Shared).GetOrAdd(value);
}
