using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    public static string CutAfterLast(this string value, ReadOnlySpan<char> needle)
        => TryCutAfterLast(value.AsSpan(), needle, out var result) ? result : value;
    public static string CutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle)
        => TryCutAfterLast(value, needle, out var result) ? result : value.ToString();
    public static string CutAfterLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType)
        => TryCutAfterLast(value.AsSpan(), needle, comparisonType, out var result) ? result : value;
    public static string CutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType)
        => TryCutAfterLast(value, needle, comparisonType, out var result) ? result : value.ToString();
    public static string CutAfterLast(this string value, ReadOnlySpan<char> needle, StringPool? pool)
        => TryCutAfterLast(value.AsSpan(), needle, pool, out var result) ? result : value;
    public static string CutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringPool? pool)
        => TryCutAfterLast(value, needle, pool, out var result) ? result : value.ToString();
    public static string CutAfterLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
        => TryCutAfterLast(value.AsSpan(), needle, comparisonType, pool, out var result) ? result : value;
    public static string CutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
        => TryCutAfterLast(value, needle, comparisonType, pool, out var result) ? result : value.ToString();

    public static bool TryCutAfterLast(this string value, ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value.AsSpan(), needle, null, out result);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value, needle, null, out result);
    public static bool TryCutAfterLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value.AsSpan(), needle, comparisonType, null, out result);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value, needle, comparisonType, null, out result);
    public static bool TryCutAfterLast(this string value, ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value.AsSpan(), needle, StringComparison.Ordinal, pool, out result);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value, needle, StringComparison.Ordinal, pool, out result);
    public static bool TryCutAfterLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value.AsSpan(), needle, comparisonType, pool, out result);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
    {
        if (TryCutSpanAfterLast(value, needle, out var cutValue))
        {
            result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
            return true;
        }
        result = null;
        return false;
    }

    public static bool TryCutAfterLast(this string value, ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
        => TryCutAfterLast(value.AsSpan(), needle, StringComparison.Ordinal, destination, out charsWritten);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
        => TryCutAfterLast(value, needle, StringComparison.Ordinal, destination, out charsWritten);
    public static bool TryCutAfterLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
        => TryCutAfterLast(value.AsSpan(), needle, comparisonType, destination, out charsWritten);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
    {
        if (TryCutSpanAfterLast(value, needle, out var cutValue) && cutValue.TryCopyTo(destination))
        {
            charsWritten = cutValue.Length;
            return true;
        }
        charsWritten = 0;
        return false;
    }

    public static ReadOnlySpan<char> CutSpanAfterLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
        => TryCutSpanAfterLast(value.AsSpan(), needle, comparisonType, out var cutValue) ? cutValue : value;
    public static ReadOnlySpan<char> CutSpanAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
        => TryCutSpanAfterLast(value, needle, comparisonType, out var cutValue) ? cutValue : value;

    public static bool TryCutSpanAfterLast(this string value, ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
        => TryCutSpanAfterLast(value.AsSpan(), needle, StringComparison.Ordinal, out cutValue);
    public static bool TryCutSpanAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
        => TryCutSpanAfterLast(value, needle, StringComparison.Ordinal, out cutValue);
    public static bool TryCutSpanAfterLast(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
        => TryCutSpanAfterLast(value.AsSpan(), needle, comparisonType, out cutValue);
    public static bool TryCutSpanAfterLast(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
    {
        cutValue = default;
        if (value.IsEmpty) return false;
        var index = value.LastIndexOf(needle, comparisonType);
        if (index == -1) return false;
        cutValue = value[(index + needle.Length)..];
        return true;
    }
}
