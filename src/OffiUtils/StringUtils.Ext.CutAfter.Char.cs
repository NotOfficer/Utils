using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public string CutAfter(char needle, StringPool? pool = null)
            => value.AsSpan().TryCutAfter(needle, pool, out string? result) ? result : value;

        public bool TryCutAfter(char needle, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfter(needle, null, out result);

        public bool TryCutAfter(char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutAfter(needle, pool, out result);

        public bool TryCutAfter(char needle, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutAfter(needle, destination, out charsWritten);

        public ReadOnlySpan<char> CutSpanAfter(char needle)
            => value.AsSpan().TryCutSpanAfter(needle, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanAfter(char needle, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanAfter(needle, out cutValue);
    }


    extension(ReadOnlySpan<char> value)
    {
        public string CutAfter(char needle, StringPool? pool = null)
            => value.TryCutAfter(needle, pool, out string? result) ? result : value.ToString();

        public bool TryCutAfter(char needle, [NotNullWhen(true)] out string? result)
            => value.TryCutAfter(needle, null, out result);

        public bool TryCutAfter(char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        {
            if (value.TryCutSpanAfter(needle, out ReadOnlySpan<char> cutValue))
            {
                result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
                return true;
            }
            result = null;
            return false;
        }

        public bool TryCutAfter(char needle, Span<char> destination, out int charsWritten)
        {
            if (value.TryCutSpanAfter(needle, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
            {
                charsWritten = cutValue.Length;
                return true;
            }
            charsWritten = 0;
            return false;
        }

        public ReadOnlySpan<char> CutSpanAfter(char needle)
            => value.TryCutSpanAfter(needle, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanAfter(char needle, out ReadOnlySpan<char> cutValue)
        {
            cutValue = default;
            if (value.IsEmpty) return false;
            int index = value.IndexOf(needle);
            if (index == -1) return false;
            cutValue = value[(index + 1)..];
            return true;
        }
    }
}
