using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    public static string CutBeforeLast(this string value, ReadOnlySpan<char> needle)
        => TryCutBeforeLast(value.AsSpan(), needle, out var result) ? result : value;
    public static string CutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle)
        => TryCutBeforeLast(value, needle, out var result) ? result : value.ToString();
    public static string CutBeforeLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType)
        => TryCutBeforeLast(value.AsSpan(), needle, comparisonType, out var result) ? result : value;
    public static string CutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType)
        => TryCutBeforeLast(value, needle, comparisonType, out var result) ? result : value.ToString();
    public static string CutBeforeLast(this string value, ReadOnlySpan<char> needle, StringPool? pool)
        => TryCutBeforeLast(value.AsSpan(), needle, pool, out var result) ? result : value;
    public static string CutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringPool? pool)
        => TryCutBeforeLast(value, needle, pool, out var result) ? result : value.ToString();
    public static string CutBeforeLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
        => TryCutBeforeLast(value.AsSpan(), needle, comparisonType, pool, out var result) ? result : value;
    public static string CutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
        => TryCutBeforeLast(value, needle, comparisonType, pool, out var result) ? result : value.ToString();

    public static bool TryCutBeforeLast(this string value, ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
        => TryCutBeforeLast(value.AsSpan(), needle, null, out result);
    public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
        => TryCutBeforeLast(value, needle, null, out result);
    public static bool TryCutBeforeLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
        => TryCutBeforeLast(value.AsSpan(), needle, comparisonType, null, out result);
    public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
        => TryCutBeforeLast(value, needle, comparisonType, null, out result);
    public static bool TryCutBeforeLast(this string value, ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutBeforeLast(value.AsSpan(), needle, StringComparison.Ordinal, pool, out result);
    public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutBeforeLast(value, needle, StringComparison.Ordinal, pool, out result);
    public static bool TryCutBeforeLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutBeforeLast(value.AsSpan(), needle, comparisonType, pool, out result);
    public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
    {
        if (TryCutSpanBeforeLast(value, needle, out var cutValue))
        {
            result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
            return true;
        }
        result = null;
        return false;
    }

    public static bool TryCutBeforeLast(this string value, ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
        => TryCutBeforeLast(value.AsSpan(), needle, StringComparison.Ordinal, destination, out charsWritten);
    public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
        => TryCutBeforeLast(value, needle, StringComparison.Ordinal, destination, out charsWritten);
    public static bool TryCutBeforeLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
        => TryCutBeforeLast(value.AsSpan(), needle, comparisonType, destination, out charsWritten);
    public static bool TryCutBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
    {
        if (TryCutSpanBeforeLast(value, needle, out var cutValue) && cutValue.TryCopyTo(destination))
        {
            charsWritten = cutValue.Length;
            return true;
        }
        charsWritten = 0;
        return false;
    }

    public static ReadOnlySpan<char> CutSpanBeforeLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
        => TryCutSpanBeforeLast(value.AsSpan(), needle, comparisonType, out var cutValue) ? cutValue : value;
    public static ReadOnlySpan<char> CutSpanBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
        => TryCutSpanBeforeLast(value, needle, comparisonType, out var cutValue) ? cutValue : value;

    public static bool TryCutSpanBeforeLast(this string value, ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
        => TryCutSpanBeforeLast(value.AsSpan(), needle, StringComparison.Ordinal, out cutValue);
    public static bool TryCutSpanBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
        => TryCutSpanBeforeLast(value, needle, StringComparison.Ordinal, out cutValue);
    public static bool TryCutSpanBeforeLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
        => TryCutSpanBeforeLast(value.AsSpan(), needle, comparisonType, out cutValue);
    public static bool TryCutSpanBeforeLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
    {
        cutValue = default;
        if (value.IsEmpty) return false;
        var index = value.LastIndexOf(needle, comparisonType);
        if (index == -1) return false;
        cutValue = value[..index];
        return true;
    }
}
