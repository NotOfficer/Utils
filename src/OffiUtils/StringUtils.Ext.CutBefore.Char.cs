using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    public static string CutBefore(this string value, char needle, StringPool? pool = null)
        => TryCutBefore(value.AsSpan(), needle, pool, out var result) ? result : value;
    public static string CutBefore(this ReadOnlySpan<char> value, char needle, StringPool? pool = null)
        => TryCutBefore(value, needle, pool, out var result) ? result : value.ToString();

    public static bool TryCutBefore(this string value, char needle, [NotNullWhen(true)] out string? result)
        => TryCutBefore(value.AsSpan(), needle, null, out result);
    public static bool TryCutBefore(this ReadOnlySpan<char> value, char needle, [NotNullWhen(true)] out string? result)
        => TryCutBefore(value, needle, null, out result);
    public static bool TryCutBefore(this string value, char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        => TryCutBefore(value.AsSpan(), needle, pool, out result);
    public static bool TryCutBefore(this ReadOnlySpan<char> value, char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
    {
        if (TryCutSpanBefore(value, needle, out var cutValue))
        {
            result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
            return true;
        }
        result = null;
        return false;
    }

    public static bool TryCutBefore(this string value, char needle, Span<char> destination, out int charsWritten)
        => TryCutBefore(value.AsSpan(), needle, destination, out charsWritten);
    public static bool TryCutBefore(this ReadOnlySpan<char> value, char needle, Span<char> destination, out int charsWritten)
    {
        if (TryCutSpanBefore(value, needle, out var cutValue) && cutValue.TryCopyTo(destination))
        {
            charsWritten = cutValue.Length;
            return true;
        }
        charsWritten = 0;
        return false;
    }

    public static ReadOnlySpan<char> CutSpanBefore(this string value, char needle)
        => TryCutSpanBefore(value.AsSpan(), needle, out var cutValue) ? cutValue : value;
    public static ReadOnlySpan<char> CutSpanBefore(this ReadOnlySpan<char> value, char needle)
        => TryCutSpanBefore(value, needle, out var cutValue) ? cutValue : value;

    public static bool TryCutSpanBefore(this string value, char needle, out ReadOnlySpan<char> cutValue)
        => TryCutSpanBefore(value.AsSpan(), needle, out cutValue);
    public static bool TryCutSpanBefore(this ReadOnlySpan<char> value, char needle, out ReadOnlySpan<char> cutValue)
    {
        cutValue = default;
        if (value.IsEmpty) return false;
        var index = value.IndexOf(needle);
        if (index == -1) return false;
        cutValue = value[..index];
        return true;
    }
}
