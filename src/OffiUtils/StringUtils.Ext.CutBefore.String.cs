using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public string CutBefore(ReadOnlySpan<char> needle)
            => value.AsSpan().TryCutBefore(needle, out string? result) ? result : value;

        public string CutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType)
            => value.AsSpan().TryCutBefore(needle, comparisonType, out string? result) ? result : value;

        public string CutBefore(ReadOnlySpan<char> needle, StringPool? pool)
            => value.AsSpan().TryCutBefore(needle, pool, out string? result) ? result : value;

        public string CutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
            => value.AsSpan().TryCutBefore(needle, comparisonType, pool, out string? result) ? result : value;

        public bool TryCutBefore(ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBefore(needle, null, out result);

        public bool TryCutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBefore(needle, comparisonType, null, out result);

        public bool TryCutBefore(ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBefore(needle, StringComparison.Ordinal, pool, out result);

        public bool TryCutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBefore(needle, comparisonType, pool, out result);

        public bool TryCutBefore(ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutBefore(needle, StringComparison.Ordinal, destination, out charsWritten);

        public bool TryCutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutBefore(needle, comparisonType, destination, out charsWritten);

        public ReadOnlySpan<char> CutSpanBefore(ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
            => value.AsSpan().TryCutSpanBefore(needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanBefore(ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanBefore(needle, StringComparison.Ordinal, out cutValue);

        public bool TryCutSpanBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanBefore(needle, comparisonType, out cutValue);
    }

    extension(ReadOnlySpan<char> value)
    {
        public string CutBefore(ReadOnlySpan<char> needle)
            => value.TryCutBefore(needle, out string? result) ? result : value.ToString();

        public string CutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType)
            => value.TryCutBefore(needle, comparisonType, out string? result) ? result : value.ToString();

        public string CutBefore(ReadOnlySpan<char> needle, StringPool? pool)
            => value.TryCutBefore(needle, pool, out string? result) ? result : value.ToString();

        public string CutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool)
            => value.TryCutBefore(needle, comparisonType, pool, out string? result) ? result : value.ToString();

        public bool TryCutBefore(ReadOnlySpan<char> needle, [NotNullWhen(true)] out string? result)
            => value.TryCutBefore(needle, null, out result);

        public bool TryCutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, [NotNullWhen(true)] out string? result)
            => value.TryCutBefore(needle, comparisonType, null, out result);

        public bool TryCutBefore(ReadOnlySpan<char> needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.TryCutBefore(needle, StringComparison.Ordinal, pool, out result);

        public bool TryCutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, StringPool? pool, [NotNullWhen(true)] out string? result)
        {
            if (value.TryCutSpanBefore(needle, out ReadOnlySpan<char> cutValue))
            {
                result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
                return true;
            }
            result = null;
            return false;
        }

        public bool TryCutBefore(ReadOnlySpan<char> needle, Span<char> destination, out int charsWritten)
            => value.TryCutBefore(needle, StringComparison.Ordinal, destination, out charsWritten);

        public bool TryCutBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, Span<char> destination, out int charsWritten)
        {
            if (value.TryCutSpanBefore(needle, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
            {
                charsWritten = cutValue.Length;
                return true;
            }
            charsWritten = 0;
            return false;
        }

        public ReadOnlySpan<char> CutSpanBefore(ReadOnlySpan<char> needle, StringComparison comparisonType = StringComparison.Ordinal)
            => value.TryCutSpanBefore(needle, comparisonType, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanBefore(ReadOnlySpan<char> needle, out ReadOnlySpan<char> cutValue)
            => value.TryCutSpanBefore(needle, StringComparison.Ordinal, out cutValue);

        public bool TryCutSpanBefore(ReadOnlySpan<char> needle, StringComparison comparisonType, out ReadOnlySpan<char> cutValue)
        {
            cutValue = default;
            if (value.IsEmpty) return false;
            int index = value.IndexOf(needle, comparisonType);
            if (index == -1) return false;
            cutValue = value[..index];
            return true;
        }
    }
}
