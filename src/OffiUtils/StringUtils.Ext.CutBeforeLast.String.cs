using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public string CutBeforeLast(ReadOnlySpan<char> needle)
            => value.AsSpan().TryCutBeforeLast(needle, out string? result) ? result : value;

        public string CutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType)
            => value.AsSpan().TryCutBeforeLast(needle, comparisonType, out string? result) ? result : value;

        public string CutBeforeLast(ReadOnlySpan<char> needle, StringPool? pool)
            => value.AsSpan().TryCutBeforeLast(needle, pool, out string? result) ? result : value;

        public string CutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
            => value.AsSpan().TryCutBeforeLast(needle, comparisonType, pool, out string? result) ? result : value;

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBeforeLast(needle, null, out result);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBeforeLast(needle, comparisonType, null, out result);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBeforeLast(needle, StringComparison.Ordinal, pool, out result);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBeforeLast(needle, comparisonType, pool, out result);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutBeforeLast(needle, StringComparison.Ordinal, destination, out charsWritten);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutBeforeLast(needle, comparisonType, destination, out charsWritten);

        public ReadOnlySpan<char> CutSpanBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
            => value.AsSpan().TryCutSpanBeforeLast(needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanBeforeLast(ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanBeforeLast(needle, StringComparison.Ordinal, out cutValue);

        public bool TryCutSpanBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanBeforeLast(needle, comparisonType, out cutValue);
    }

    extension(ReadOnlySpan<char> value)
    {
        public string CutBeforeLast(ReadOnlySpan<char> needle)
            => value.TryCutBeforeLast(needle, out string? result) ? result : value.ToString();

        public string CutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType)
            => value.TryCutBeforeLast(needle, comparisonType, out string? result) ? result : value.ToString();

        public string CutBeforeLast(ReadOnlySpan<char> needle, StringPool? pool)
            => value.TryCutBeforeLast(needle, pool, out string? result) ? result : value.ToString();

        public string CutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
            => value.TryCutBeforeLast(needle, comparisonType, pool, out string? result) ? result : value.ToString();

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
            => value.TryCutBeforeLast(needle, null, out result);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
            => value.TryCutBeforeLast(needle, comparisonType, null, out result);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.TryCutBeforeLast(needle, StringComparison.Ordinal, pool, out result);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
        {
            if (value.TryCutSpanBeforeLast(needle, out ReadOnlySpan<char> cutValue))
            {
                result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
                return true;
            }
            result = null;
            return false;
        }

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
            => value.TryCutBeforeLast(needle, StringComparison.Ordinal, destination, out charsWritten);

        public bool TryCutBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
        {
            if (value.TryCutSpanBeforeLast(needle, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
            {
                charsWritten = cutValue.Length;
                return true;
            }
            charsWritten = 0;
            return false;
        }

        public ReadOnlySpan<char> CutSpanBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
            => value.TryCutSpanBeforeLast(needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanBeforeLast(ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
            => value.TryCutSpanBeforeLast(needle, StringComparison.Ordinal, out cutValue);

        public bool TryCutSpanBeforeLast(ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
        {
            cutValue = default;
            if (value.IsEmpty) return false;
            int index = value.LastIndexOf(needle, comparisonType);
            if (index == -1) return false;
            cutValue = value[..index];
            return true;
        }
    }
}
