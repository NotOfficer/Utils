using System.Diagnostics.CodeAnalysis;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static partial class StringUtils
{
    extension(string value)
    {
        public string CutBefore(char needle, StringPool? pool = null)
            => value.AsSpan().TryCutBefore(needle, pool, out string? result) ? result : value;

        public bool TryCutBefore(char needle, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBefore(needle, null, out result);

        public bool TryCutBefore(char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
            => value.AsSpan().TryCutBefore(needle, pool, out result);

        public bool TryCutBefore(char needle, Span<char> destination, out int charsWritten)
            => value.AsSpan().TryCutBefore(needle, destination, out charsWritten);

        public ReadOnlySpan<char> CutSpanBefore(char needle)
            => value.AsSpan().TryCutSpanBefore(needle, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanBefore(char needle, out ReadOnlySpan<char> cutValue)
            => value.AsSpan().TryCutSpanBefore(needle, out cutValue);
    }

    extension(ReadOnlySpan<char> value)
    {
        public string CutBefore(char needle, StringPool? pool = null)
            => value.TryCutBefore(needle, pool, out string? result) ? result : value.ToString();

        public bool TryCutBefore(char needle, [NotNullWhen(true)] out string? result)
            => value.TryCutBefore(needle, null, out result);

        public bool TryCutBefore(char needle, StringPool? pool, [NotNullWhen(true)] out string? result)
        {
            if (value.TryCutSpanBefore(needle, out ReadOnlySpan<char> cutValue))
            {
                result = pool is null ? cutValue.ToString() : pool.GetOrAdd(cutValue);
                return true;
            }
            result = null;
            return false;
        }

        public bool TryCutBefore(char needle, Span<char> destination, out int charsWritten)
        {
            if (value.TryCutSpanBefore(needle, out ReadOnlySpan<char> cutValue) && cutValue.TryCopyTo(destination))
            {
                charsWritten = cutValue.Length;
                return true;
            }
            charsWritten = 0;
            return false;
        }

        public ReadOnlySpan<char> CutSpanBefore(char needle)
            => value.TryCutSpanBefore(needle, out ReadOnlySpan<char> cutValue) ? cutValue : value;

        public bool TryCutSpanBefore(char needle, out ReadOnlySpan<char> cutValue)
        {
            cutValue = default;
            if (value.IsEmpty) return false;
            int index = value.IndexOf(needle);
            if (index == -1) return false;
            cutValue = value[..index];
            return true;
        }
    }
}
