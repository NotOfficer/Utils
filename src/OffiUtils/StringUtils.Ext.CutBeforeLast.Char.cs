using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
	public static string CutBeforeLast(this string value, char needle, StringPool? pool = null)
		=> TryCutBeforeLast(value.AsSpan(), needle, pool, out var result) ? result : value;
	public static string CutBeforeLast(this ReadOnlySpan<char> value, char needle, StringPool? pool = null)
		=> TryCutBeforeLast(value, needle, pool, out var result) ? result : value.ToString();

	public static bool TryCutBeforeLast(this string value, char needle, [NotNullWhen(true)] out string? result)
		=> TryCutBeforeLast(value.AsSpan(), needle, null, out result);
	public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, char needle, [NotNullWhen(true)] out string? result)
		=> TryCutBeforeLast(value, needle, null, out result);
	public static bool TryCutBeforeLast(this string value, char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
		=> TryCutBeforeLast(value.AsSpan(), needle, pool, out result);
	public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
	{
		if (TryCutSpanBeforeLast(value, needle, out var cutValue))
		{
			result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
			return true;
		}
		result = null;
		return false;
	}

	public static bool TryCutBeforeLast(this string value, char needle, Span<char> destination, out int charsWritten)
		=> TryCutBeforeLast(value.AsSpan(), needle, destination, out charsWritten);
	public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, char needle, Span<char> destination, out int charsWritten)
	{
		if (TryCutSpanBeforeLast(value, needle, out var cutValue) && cutValue.TryCopyTo(destination))
		{
			charsWritten = cutValue.Length;
			return true;
		}
		charsWritten = 0;
		return false;
	}

	public static ReadOnlySpan<char> CutSpanBeforeLast(this string value, char needle)
		=> TryCutSpanBeforeLast(value.AsSpan(), needle, out var cutValue) ? cutValue : value;
	public static ReadOnlySpan<char> CutSpanBeforeLast(this ReadOnlySpan<char> value, char needle)
		=> TryCutSpanBeforeLast(value, needle, out var cutValue) ? cutValue : value;

	public static bool TryCutSpanBeforeLast(this string value, char needle, out ReadOnlySpan<char> cutValue)
		=> TryCutSpanBeforeLast(value.AsSpan(), needle, out cutValue);
	public static bool TryCutSpanBeforeLast(this ReadOnlySpan<char> value, char needle, out ReadOnlySpan<char> cutValue)
	{
		cutValue = default;
		if (value.IsEmpty) return false;
		var index = value.LastIndexOf(needle);
		if (index == -1) return false;
		cutValue = value[..index];
		return true;
	}
}
