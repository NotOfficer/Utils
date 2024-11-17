using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
	public static string? CutAfter(this string value, char needle, StringPool? pool = null)
		=> TryCutAfter(value.AsSpan(), needle, pool, out var result) ? result : null;
	public static string? CutAfter(this ReadOnlySpan<char> value, char needle, StringPool? pool = null)
		=> TryCutAfter(value, needle, pool, out var result) ? result : null;

	public static bool TryCutAfter(this string value, char needle, [NotNullWhen(true)] out string? result)
		=> TryCutAfter(value.AsSpan(), needle, null, out result);
	public static bool TryCutAfter(this ReadOnlySpan<char> value, char needle, [NotNullWhen(true)] out string? result)
		=> TryCutAfter(value, needle, null, out result);
	public static bool TryCutAfter(this string value, char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
		=> TryCutAfter(value.AsSpan(), needle, pool, out result);
	public static bool TryCutAfter(this ReadOnlySpan<char> value, char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
	{
		if (TryCutSpanAfter(value, needle, out var cutValue))
		{
			result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
			return true;
		}
		result = null;
		return false;
	}

	public static bool TryCutAfter(this string value, char needle, Span<char> destination, out int charsWritten)
		=> TryCutAfter(value.AsSpan(), needle, destination, out charsWritten);
	public static bool TryCutAfter(this ReadOnlySpan<char> value, char needle, Span<char> destination, out int charsWritten)
	{
		if (TryCutSpanAfter(value, needle, out var cutValue) && cutValue.TryCopyTo(destination))
		{
			charsWritten = cutValue.Length;
			return true;
		}
		charsWritten = 0;
		return false;
	}

	public static ReadOnlySpan<char> CutSpanAfter(this string value, char needle)
		=> TryCutSpanAfter(value.AsSpan(), needle, out var cutValue) ? cutValue : default;
	public static ReadOnlySpan<char> CutSpanAfter(this ReadOnlySpan<char> value, char needle)
		=> TryCutSpanAfter(value, needle, out var cutValue) ? cutValue : default;

	public static bool TryCutSpanAfter(this string value, char needle, out ReadOnlySpan<char> cutValue)
		=> TryCutSpanAfter(value.AsSpan(), needle, out cutValue);
	public static bool TryCutSpanAfter(this ReadOnlySpan<char> value, char needle, out ReadOnlySpan<char> cutValue)
	{
		cutValue = default;
		if (value.IsEmpty) return false;
		var index = value.IndexOf(needle);
		if (index == -1) return false;
		cutValue = value[(index + 1)..];
		return true;
	}
}
