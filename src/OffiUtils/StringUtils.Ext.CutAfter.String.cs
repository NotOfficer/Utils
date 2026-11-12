using System.Diagnostics.CodeAnalysis;
using System.Globalization;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public string CutAfter(ReadOnlySpan<char> needle)
            => value.AsSpan().TryCutAfter(needle, out string? result) ? result : value;

        public string CutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType)
            => value.AsSpan().TryCutAfter(needle, comparisonType, out string? result) ? result : value;

        public string CutAfter(ReadOnlySpan<char> needle, StringPool? pool)
            => value.AsSpan().TryCutAfter(needle, pool, out string? result) ? result : value;

        public string CutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
            => value.AsSpan().TryCutAfter(needle, comparisonType, pool, out string? result) ? result : value;

        public bool TryCutAfter(ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfter(needle, null, out result);

        public bool TryCutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfter(needle, comparisonType, null, out result);

        public bool TryCutAfter(ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfter(needle, StringComparison.Ordinal, pool, out result);

        public bool TryCutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfter(needle, comparisonType, pool, out result);

        public bool TryCutAfter(ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutAfter(needle, StringComparison.Ordinal, destination, out charsWritten);

        public bool TryCutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutAfter(needle, comparisonType, destination, out charsWritten);

        public ReadOnlySpan<char> CutSpanAfter(ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
            => value.AsSpan().TryCutSpanAfter(needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanAfter(ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanAfter(needle, StringComparison.Ordinal, out cutValue);

        public bool TryCutSpanAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanAfter(needle, comparisonType, out cutValue);
    }


    extension(ReadOnlySpan<char> value)
    {
        public string CutAfter(ReadOnlySpan<char> needle)
            => value.TryCutAfter(needle, out string? result) ? result : value.ToString();

        public string CutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType)
            => value.TryCutAfter(needle, comparisonType, out string? result) ? result : value.ToString();

        public string CutAfter(ReadOnlySpan<char> needle, StringPool? pool)
            => value.TryCutAfter(needle, pool, out string? result) ? result : value.ToString();

        public string CutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
            => value.TryCutAfter(needle, comparisonType, pool, out string? result) ? result : value.ToString();

        public bool TryCutAfter(ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
            => value.TryCutAfter(needle, null, out result);

        public bool TryCutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
            => value.TryCutAfter(needle, comparisonType, null, out result);

        public bool TryCutAfter(ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.TryCutAfter(needle, StringComparison.Ordinal, pool, out result);

        public bool TryCutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
        {
            if (value.TryCutSpanAfter(needle, comparisonType, out ReadOnlySpan<char> cutValue))
            {
                result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
                return true;
            }
            result = null;
            return false;
        }

        public bool TryCutAfter(ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
            => value.TryCutAfter(needle, StringComparison.Ordinal, destination, out charsWritten);

        public bool TryCutAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
        {
            if (value.TryCutSpanAfter(needle, comparisonType, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
            {
                charsWritten = cutValue.Length;
                return true;
            }
            charsWritten = 0;
            return false;
        }

        public ReadOnlySpan<char> CutSpanAfter(ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
            => value.TryCutSpanAfter(needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanAfter(ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
            => value.TryCutSpanAfter(needle, StringComparison.Ordinal, out cutValue);

        public bool TryCutSpanAfter(ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
        {
            cutValue = default;
            if (value.IsEmpty) return false;
            int index = IndexOf(value, needle, comparisonType, out int matchLength);
            if (index == -1) return false;
            cutValue = value[(index + matchLength)..];
            return true;
        }
    }

    // culture-sensitive matches can be longer or shorter than the needle (ignorable characters like U+00AD,
    // or "\u00C5" matching "A\u030A"), so cutting after a match needs the length of the matched text
    private static int IndexOf(ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, out int matchLength)
    {
        if (comparisonType is StringComparison.Ordinal or StringComparison.OrdinalIgnoreCase)
        {
            matchLength = needle.Length;
            return value.IndexOf(needle, comparisonType);
        }

        (CompareInfo compareInfo, CompareOptions options) = GetCompareInfo(comparisonType);
        return compareInfo.IndexOf(value, needle, options, out matchLength);
    }

    private static int LastIndexOf(ReadOnlySpan<char> value, ReadOnlySpan<char> needle, StringComparison comparisonType, out int matchLength)
    {
        if (comparisonType is StringComparison.Ordinal or StringComparison.OrdinalIgnoreCase)
        {
            matchLength = needle.Length;
            return value.LastIndexOf(needle, comparisonType);
        }

        (CompareInfo compareInfo, CompareOptions options) = GetCompareInfo(comparisonType);
        return compareInfo.LastIndexOf(value, needle, options, out matchLength);
    }

    private static (CompareInfo CompareInfo, CompareOptions Options) GetCompareInfo(StringComparison comparisonType) => comparisonType switch
    {
        StringComparison.CurrentCulture => (CultureInfo.CurrentCulture.CompareInfo, CompareOptions.None),
        StringComparison.CurrentCultureIgnoreCase => (CultureInfo.CurrentCulture.CompareInfo, CompareOptions.IgnoreCase),
        StringComparison.InvariantCulture => (CultureInfo.InvariantCulture.CompareInfo, CompareOptions.None),
        StringComparison.InvariantCultureIgnoreCase => (CultureInfo.InvariantCulture.CompareInfo, CompareOptions.IgnoreCase),
        _ => throw new ArgumentException("The string comparison type passed in is currently not supported.", nameof(comparisonType))
    };
}
