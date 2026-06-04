using System.Security.Cryptography;

namespace OffiUtils;

public static partial class StringUtils
{
    public const string RandomPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890123456789012345678901234567890123456789";

    private static void FastAllocateCreateAction(Span<char> str, int arg) { }

    public static string FastAllocate(int length) => string.Create(length, length, FastAllocateCreateAction);

    public static string BytesToHexUpper(ReadOnlySpan<byte> bytes) => Convert.ToHexString(bytes);
    public static string BytesToHexLower(ReadOnlySpan<byte> bytes) => Convert.ToHexStringLower(bytes);

    public static bool TryWriteBytesToHexLower(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => Convert.TryToHexStringLower(bytes, destination, out charsWritten);
    public static bool TryWriteBytesToHexUpper(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => Convert.TryToHexString(bytes, destination, out charsWritten);

    public static string Random(int length, ReadOnlySpan<char> pool) => RandomNumberGenerator.GetString(pool, length);
    public static string Random(int length) => RandomNumberGenerator.GetString(RandomPool, length);

    public static char ToLowerAsciiInvariant(char c)
    {
        if (char.IsAsciiLetterUpper(c))
        {
            // on x86, extending BYTE -> DWORD is more efficient than WORD -> DWORD
            c = (char)(byte)(c | 0x20);
        }
        return c;
    }

    public static char ToUpperAsciiInvariant(char c)
    {
        if (char.IsAsciiLetterLower(c))
        {
            c = (char)(c & 0x5F); // = low 7 bits of ~0x20
        }
        return c;
    }

    public static void ToLowerAsciiInvariant(ReadOnlySpan<char> value, Span<char> destination)
    {
        for (int i = 0; i < value.Length; i++)
        {
            destination[i] = ToLowerAsciiInvariant(value[i]);
        }
    }

    public static void ToUpperAsciiInvariant(ReadOnlySpan<char> value, Span<char> destination)
    {
        for (int i = 0; i < value.Length; i++)
        {
            destination[i] = ToUpperAsciiInvariant(value[i]);
        }
    }

    public static void ToLowerAsciiInvariant(string value)
    {
        if (value.Length == 0) return;
        Span<char> span = value.GetSpan();
        ToLowerAsciiInvariant(span, span);
    }

    public static void ToUpperAsciiInvariant(string value)
    {
        if (value.Length == 0) return;
        Span<char> span = value.GetSpan();
        ToUpperAsciiInvariant(span, span);
    }

    public static string RealClone(ReadOnlySpan<char> value)
        => value.ToString();
}
