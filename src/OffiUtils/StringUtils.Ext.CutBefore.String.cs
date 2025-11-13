using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
	public static string CutBefore(this string value, ReadOnlySpan<char> needle)
		=> TryCutBefore(value.AsSpan(), needle, out var result) ? result : value;
	public static string CutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle)
		=> TryCutBefore(value, needle, out var result) ? result : value.ToString();
	public static string CutBefore(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType)
		=> TryCutBefore(value.AsSpan(), needle, comparisonType, out var result) ? result : value;
	public static string CutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType)
		=> TryCutBefore(value, needle, comparisonType, out var result) ? result : value.ToString();
	public static string CutBefore(this string value, ReadOnlySpan<char> needle, StringPool? pool)
		=> TryCutBefore(value.AsSpan(), needle, pool, out var result) ? result : value;
	public static string CutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringPool? pool)
		=> TryCutBefore(value, needle, pool, out var result) ? result : value.ToString();
	public static string CutBefore(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
		=> TryCutBefore(value.AsSpan(), needle, comparisonType, pool, out var result) ? result : value;
	public static string CutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
		=> TryCutBefore(value, needle, comparisonType, pool, out var result) ? result : value.ToString();

	public static bool TryCutBefore(this string value, ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
		=> TryCutBefore(value.AsSpan(), needle, null, out result);
	public static bool TryCutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
		=> TryCutBefore(value, needle, null, out result);
	public static bool TryCutBefore(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
		=> TryCutBefore(value.AsSpan(), needle, comparisonType, null, out result);
	public static bool TryCutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
		=> TryCutBefore(value, needle, comparisonType, null, out result);
	public static bool TryCutBefore(this string value, ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
		=> TryCutBefore(value.AsSpan(), needle, StringComparison.Ordinal, pool, out result);
	public static bool TryCutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
		=> TryCutBefore(value, needle, StringComparison.Ordinal, pool, out result);
	public static bool TryCutBefore(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
		=> TryCutBefore(value.AsSpan(), needle, comparisonType, pool, out result);
	public static bool TryCutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
	{
		if (TryCutSpanBefore(value, needle, out var cutValue))
		{
			result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
			return true;
		}
		result = null;
		return false;
	}

	public static bool TryCutBefore(this string value, ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
		=> TryCutBefore(value.AsSpan(), needle, StringComparison.Ordinal, destination, out charsWritten);
	public static bool TryCutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
		=> TryCutBefore(value, needle, StringComparison.Ordinal, destination, out charsWritten);
	public static bool TryCutBefore(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
		=> TryCutBefore(value.AsSpan(), needle, comparisonType, destination, out charsWritten);
	public static bool TryCutBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
	{
		if (TryCutSpanBefore(value, needle, out var cutValue) && cutValue.TryCopyTo(destination))
		{
			charsWritten = cutValue.Length;
			return true;
		}
		charsWritten = 0;
		return false;
	}

	public static ReadOnlySpan<char> CutSpanBefore(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
		=> TryCutSpanBefore(value.AsSpan(), needle, comparisonType, out var cutValue) ? cutValue : value;
	public static ReadOnlySpan<char> CutSpanBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
		=> TryCutSpanBefore(value, needle, comparisonType, out var cutValue) ? cutValue : value;

	public static bool TryCutSpanBefore(this string value, ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
		=> TryCutSpanBefore(value.AsSpan(), needle, StringComparison.Ordinal, out cutValue);
	public static bool TryCutSpanBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
		=> TryCutSpanBefore(value, needle, StringComparison.Ordinal, out cutValue);
	public static bool TryCutSpanBefore(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
		=> TryCutSpanBefore(value.AsSpan(), needle, comparisonType, out cutValue);
	public static bool TryCutSpanBefore(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
	{
		cutValue = default;
		if (value.IsEmpty) return false;
		var index = value.IndexOf(needle, comparisonType);
		if (index == -1) return false;
		cutValue = value[..index];
		return true;
	}
}
