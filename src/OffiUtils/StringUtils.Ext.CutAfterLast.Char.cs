using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public string CutAfterLast(char needle, StringPool? pool = null)
            => value.AsSpan().TryCutAfterLast(needle, pool, out string? result) ? result : value;

        public bool TryCutAfterLast(char needle, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfterLast(needle, null, out result);

        public bool TryCutAfterLast(char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfterLast(needle, pool, out result);

        public bool TryCutAfterLast(char needle, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutAfterLast(needle, destination, out charsWritten);

        public ReadOnlySpan<char> CutSpanAfterLast(char needle)
            => value.AsSpan().TryCutSpanAfterLast(needle, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanAfterLast(char needle, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanAfterLast(needle, out cutValue);
    }


    extension(ReadOnlySpan<char> value)
    {
        public string CutAfterLast(char needle, StringPool? pool = null)
            => value.TryCutAfterLast(needle, pool, out string? result) ? result : value.ToString();

        public bool TryCutAfterLast(char needle, [NotNullWhen(true)] out string? result)
            => value.TryCutAfterLast(needle, null, out result);

        public bool TryCutAfterLast(char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        {
            if (value.TryCutSpanAfterLast(needle, out ReadOnlySpan<char> cutValue))
            {
                result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
                return true;
            }
            result = null;
            return false;
        }

        public bool TryCutAfterLast(char needle, Span<char> destination, out int charsWritten)
        {
            if (value.TryCutSpanAfterLast(needle, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
            {
                charsWritten = cutValue.Length;
                return true;
            }
            charsWritten = 0;
            return false;
        }

        public ReadOnlySpan<char> CutSpanAfterLast(char needle)
            => value.TryCutSpanAfterLast(needle, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanAfterLast(char needle, out ReadOnlySpan<char> cutValue)
        {
            cutValue = default;
            if (value.IsEmpty) return false;
            int index = value.LastIndexOf(needle);
            if (index == -1) return false;
            cutValue = value[(index + 1)..];
            return true;
        }
    }
}
