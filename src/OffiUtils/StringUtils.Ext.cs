using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace OffiUtils;

public static partial class StringUtils
{
	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	public static ref char GetRawData(this string value) => ref MemoryMarshal.GetReference(value.AsSpan());

	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	public static Span<char> GetSpan(this string value) => MemoryMarshal.CreateSpan(ref GetRawData(value), value.Length);
}
