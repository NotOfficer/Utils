using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public string CutBeforeLast(char needle, StringPool? pool = null)
            => value.AsSpan().TryCutBeforeLast(needle, pool, out string? result) ? result : value;

        public bool TryCutBeforeLast(char needle, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBeforeLast(needle, null, out result);

        public bool TryCutBeforeLast(char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBeforeLast(needle, pool, out result);

        public bool TryCutBeforeLast(char needle, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutBeforeLast(needle, destination, out charsWritten);

        public ReadOnlySpan<char> CutSpanBeforeLast(char needle)
            => value.AsSpan().TryCutSpanBeforeLast(needle, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanBeforeLast(char needle, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanBeforeLast(needle, out cutValue);
    }

    extension(ReadOnlySpan<char> value)
    {
        public string CutBeforeLast(char needle, StringPool? pool = null)
            => value.TryCutBeforeLast(needle, pool, out string? result) ? result : value.ToString();

        public bool TryCutBeforeLast(char needle, [NotNullWhen(true)] out string? result)
            => value.TryCutBeforeLast(needle, null, out result);

        public bool TryCutBeforeLast(char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        {
            if (value.TryCutSpanBeforeLast(needle, out ReadOnlySpan<char> cutValue))
            {
                result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
                return true;
            }
            result = null;
            return false;
        }

        public bool TryCutBeforeLast(char needle, Span<char> destination, out int charsWritten)
        {
            if (value.TryCutSpanBeforeLast(needle, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
            {
                charsWritten = cutValue.Length;
                return true;
            }
            charsWritten = 0;
            return false;
        }

        public ReadOnlySpan<char> CutSpanBeforeLast(char needle)
            => value.TryCutSpanBeforeLast(needle, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanBeforeLast(char needle, out ReadOnlySpan<char> cutValue)
        {
            cutValue = default;
            if (value.IsEmpty) return false;
            int index = value.LastIndexOf(needle);
            if (index == -1) return false;
            cutValue = value[..index];
            return true;
        }
    }
}
