using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    public static string CutAfterLast(this string value, char needle, StringPool? pool = null)
        => TryCutAfterLast(value.AsSpan(), needle, pool, out var result) ? result : value;
    public static string CutAfterLast(this ReadOnlySpan<char> value, char needle, StringPool? pool = null)
        => TryCutAfterLast(value, needle, pool, out var result) ? result : value.ToString();

    public static bool TryCutAfterLast(this string value, char needle, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value.AsSpan(), needle, null, out result);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, char needle, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value, needle, null, out result);
    public static bool TryCutAfterLast(this string value, char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutAfterLast(value.AsSpan(), needle, pool, out result);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
    {
        if (TryCutSpanAfterLast(value, needle, out var cutValue))
        {
            result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
            return true;
        }
        result = null;
        return false;
    }

    public static bool TryCutAfterLast(this string value, char needle, Span<char> destination, out int charsWritten)
        => TryCutAfterLast(value.AsSpan(), needle, destination, out charsWritten);
    public static bool TryCutAfterLast(this ReadOnlySpan<char> value, char needle, Span<char> destination, out int charsWritten)
    {
        if (TryCutSpanAfterLast(value, needle, out var cutValue) && cutValue.TryCopyTo(destination))
        {
            charsWritten = cutValue.Length;
            return true;
        }
        charsWritten = 0;
        return false;
    }

    public static ReadOnlySpan<char> CutSpanAfterLast(this string value, char needle)
        => TryCutSpanAfterLast(value.AsSpan(), needle, out var cutValue) ? cutValue : value;
    public static ReadOnlySpan<char> CutSpanAfterLast(this ReadOnlySpan<char> value, char needle)
        => TryCutSpanAfterLast(value, needle, out var cutValue) ? cutValue : value;

    public static bool TryCutSpanAfterLast(this string value, char needle, out ReadOnlySpan<char> cutValue)
        => TryCutSpanAfterLast(value.AsSpan(), needle, out cutValue);
    public static bool TryCutSpanAfterLast(this ReadOnlySpan<char> value, char needle, out ReadOnlySpan<char> cutValue)
    {
        cutValue = default;
        if (value.IsEmpty) return false;
        var index = value.LastIndexOf(needle);
        if (index == -1) return false;
        cutValue = value[(index + 1)..];
        return true;
    }
}
