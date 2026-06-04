using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public string CutAfterLast(ReadOnlySpan<char> needle)
            => value.AsSpan().TryCutAfterLast(needle, out string? result) ? result : value;

        public string CutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType)
            => value.AsSpan().TryCutAfterLast(needle, comparisonType, out string? result) ? result : value;

        public string CutAfterLast(ReadOnlySpan<char> needle, StringPool? pool)
            => value.AsSpan().TryCutAfterLast(needle, pool, out string? result) ? result : value;

        public string CutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
            => value.AsSpan().TryCutAfterLast(needle, comparisonType, pool, out string? result) ? result : value;

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfterLast(needle, null, out result);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfterLast(needle, comparisonType, null, out result);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfterLast(needle, StringComparison.Ordinal, pool, out result);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfterLast(needle, comparisonType, pool, out result);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutAfterLast(needle, StringComparison.Ordinal, destination, out charsWritten);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutAfterLast(needle, comparisonType, destination, out charsWritten);

        public ReadOnlySpan<char> CutSpanAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
            => value.AsSpan().TryCutSpanAfterLast(needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanAfterLast(ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanAfterLast(needle, StringComparison.Ordinal, out cutValue);

        public bool TryCutSpanAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanAfterLast(needle, comparisonType, out cutValue);
    }

    extension(ReadOnlySpan<char> value)
    {
        public string CutAfterLast(ReadOnlySpan<char> needle)
            => value.TryCutAfterLast(needle, out string? result) ? result : value.ToString();

        public string CutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType)
            => value.TryCutAfterLast(needle, comparisonType, out string? result) ? result : value.ToString();

        public string CutAfterLast(ReadOnlySpan<char> needle, StringPool? pool)
            => value.TryCutAfterLast(needle, pool, out string? result) ? result : value.ToString();

        public string CutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
            => value.TryCutAfterLast(needle, comparisonType, pool, out string? result) ? result : value.ToString();

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
            => value.TryCutAfterLast(needle, null, out result);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
            => value.TryCutAfterLast(needle, comparisonType, null, out result);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.TryCutAfterLast(needle, StringComparison.Ordinal, pool, out result);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
        {
            if (value.TryCutSpanAfterLast(needle, out ReadOnlySpan<char> cutValue))
            {
                result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
                return true;
            }
            result = null;
            return false;
        }

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
            => value.TryCutAfterLast(needle, StringComparison.Ordinal, destination, out charsWritten);

        public bool TryCutAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
        {
            if (value.TryCutSpanAfterLast(needle, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
            {
                charsWritten = cutValue.Length;
                return true;
            }
            charsWritten = 0;
            return false;
        }

        public ReadOnlySpan<char> CutSpanAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
            => value.TryCutSpanAfterLast(needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanAfterLast(ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
            => value.TryCutSpanAfterLast(needle, StringComparison.Ordinal, out cutValue);

        public bool TryCutSpanAfterLast(ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
        {
            cutValue = default;
            if (value.IsEmpty) return false;
            int index = value.LastIndexOf(needle, comparisonType);
            if (index == -1) return false;
            cutValue = value[(index + needle.Length)..];
            return true;
        }
    }
}
