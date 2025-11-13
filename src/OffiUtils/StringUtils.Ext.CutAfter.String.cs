using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    public static string CutAfter(this string value, ReadOnlySpan<char> needle)
        => TryCutAfter(value.AsSpan(), needle, out string? result) ? result : value;
    public static string CutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle)
        => TryCutAfter(value, needle, out string? result) ? result : value.ToString();
    public static string CutAfter(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType)
        => TryCutAfter(value.AsSpan(), needle, comparisonType, out string? result) ? result : value;
    public static string CutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType)
        => TryCutAfter(value, needle, comparisonType, out string? result) ? result : value.ToString();
    public static string CutAfter(this string value, ReadOnlySpan<char> needle, StringPool? pool)
        => TryCutAfter(value.AsSpan(), needle, pool, out string? result) ? result : value;
    public static string CutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringPool? pool)
        => TryCutAfter(value, needle, pool, out string? result) ? result : value.ToString();
    public static string CutAfter(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
        => TryCutAfter(value.AsSpan(), needle, comparisonType, pool, out string? result) ? result : value;
    public static string CutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
        => TryCutAfter(value, needle, comparisonType, pool, out string? result) ? result : value.ToString();

    public static bool TryCutAfter(this string value, ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
        => TryCutAfter(value.AsSpan(), needle, null, out result);
    public static bool TryCutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
        => TryCutAfter(value, needle, null, out result);
    public static bool TryCutAfter(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
        => TryCutAfter(value.AsSpan(), needle, comparisonType, null, out result);
    public static bool TryCutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
        => TryCutAfter(value, needle, comparisonType, null, out result);
    public static bool TryCutAfter(this string value, ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutAfter(value.AsSpan(), needle, StringComparison.Ordinal, pool, out result);
    public static bool TryCutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutAfter(value, needle, StringComparison.Ordinal, pool, out result);
    public static bool TryCutAfter(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutAfter(value.AsSpan(), needle, comparisonType, pool, out result);
    public static bool TryCutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
    {
        if (TryCutSpanAfter(value, needle, out ReadOnlySpan<char> cutValue))
        {
            result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
            return true;
        }
        result = null;
        return false;
    }

    public static bool TryCutAfter(this string value, ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
        => TryCutAfter(value.AsSpan(), needle, StringComparison.Ordinal, destination, out charsWritten);
    public static bool TryCutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
        => TryCutAfter(value, needle, StringComparison.Ordinal, destination, out charsWritten);
    public static bool TryCutAfter(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
        => TryCutAfter(value.AsSpan(), needle, comparisonType, destination, out charsWritten);
    public static bool TryCutAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
    {
        if (TryCutSpanAfter(value, needle, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
        {
            charsWritten = cutValue.Length;
            return true;
        }
        charsWritten = 0;
        return false;
    }

    public static ReadOnlySpan<char> CutSpanAfter(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
        => TryCutSpanAfter(value.AsSpan(), needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;
    public static ReadOnlySpan<char> CutSpanAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
        => TryCutSpanAfter(value, needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

    public static bool TryCutSpanAfter(this string value, ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
        => TryCutSpanAfter(value.AsSpan(), needle, StringComparison.Ordinal, out cutValue);
    public static bool TryCutSpanAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
        => TryCutSpanAfter(value, needle, StringComparison.Ordinal, out cutValue);
    public static bool TryCutSpanAfter(this string value, ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
        => TryCutSpanAfter(value.AsSpan(), needle, comparisonType, out cutValue);
    public static bool TryCutSpanAfter(this ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
    {
        cutValue = default;
        if (value.IsEmpty) return false;
        int index = value.IndexOf(needle, comparisonType);
        if (index == -1) return false;
        cutValue = value[(index + needle.Length)..];
        return true;
    }
}
