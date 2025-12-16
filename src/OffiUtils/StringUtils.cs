using System.Runtime.CompilerServices;
using System.Security.Cryptography;

namespace OffiUtils;

public static partial class StringUtils
{
    public const string RandomPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890123456789012345678901234567890123456789";

    static StringUtils()
    {
#if !NET10_0_OR_GREATER // don't use >= 9 because of this bug in older runtimes: https://github.com/dotnet/runtime/issues/109807
        _lookup32LowerPtr = CreateLookup32Unsafe(true);
        _lookup32UpperPtr = CreateLookup32Unsafe(false);
#endif
    }

    private static void FastAllocateCreateAction(Span<char> str, int arg) { }

    public static string FastAllocate(int length) => string.Create(length, length, FastAllocateCreateAction);

    public static string BytesToHexUpper(ReadOnlySpan<byte> bytes) => Convert.ToHexString(bytes);
#if NET9_0_OR_GREATER
    public static string BytesToHexLower(ReadOnlySpan<byte> bytes) => Convert.ToHexStringLower(bytes);
#else
    public static string BytesToHexLower(ReadOnlySpan<byte> bytes) => BytesToHex(bytes, _lookup32LowerPtr);

    private static string BytesToHex(ReadOnlySpan<byte> bytes, nint lookupPtr)
    {
        string result = FastAllocate(bytes.Length * sizeof(char));
        Span<uint> resultSpan32 = System.Runtime.InteropServices.MemoryMarshal.CreateSpan(ref Unsafe.As<char, uint>(ref result.GetRawData()), bytes.Length);
        Span<uint> lookupSpan = MemoryUtils.GetSpan<uint>(lookupPtr, 256);

        for (int i = 0; i < resultSpan32.Length; i++)
        {
            resultSpan32[i] = lookupSpan[bytes[i]];
        }

        return result;
    }
#endif
#if NET10_0_OR_GREATER
    public static bool TryWriteBytesToHexLower(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => Convert.TryToHexStringLower(bytes, destination, out charsWritten);
    public static bool TryWriteBytesToHexUpper(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => Convert.TryToHexString(bytes, destination, out charsWritten);
#else
    private static readonly nint _lookup32LowerPtr;
    private static readonly nint _lookup32UpperPtr;

    private static char ConvertNibble(int nibble, int adjust) =>
        (char)(55 + adjust + nibble + (((nibble - 10) >> 31) & (-7 - adjust)));

    private static nint CreateLookup32Unsafe(bool lowerCase)
    {
        int adjust = lowerCase ? 32 : 0;
        Span<uint> span = MemoryUtils.NativeAlloc<uint>(256, out nint ptr);

        for (int i = 0; i < 256; i++)
        {
            char a = ConvertNibble(i >> 4, adjust);
            char b = ConvertNibble(i & 0xF, adjust);
            span[i] = ((uint)a) + ((uint)b << 16);
        }

        return ptr;
    }

    private static bool TryWriteBytesToHex(ReadOnlySpan<byte> bytes, nint lookupPtr, Span<char> destination, out int charsWritten)
    {
        int charsToWrite = bytes.Length * 2;
        if (destination.Length < charsToWrite)
        {
            charsWritten = 0;
            return false;
        }

        Span<uint> resultSpan32 = System.Runtime.InteropServices.MemoryMarshal.CreateSpan(ref Unsafe.As<char, uint>(ref destination.GetPinnableReference()), bytes.Length);
        Span<uint> lookupSpan = MemoryUtils.GetSpan<uint>(lookupPtr, 256);

        for (int i = 0; i < resultSpan32.Length; i++)
        {
            resultSpan32[i] = lookupSpan[bytes[i]];
        }

        charsWritten = charsToWrite;
        return true;
    }

    public static bool TryWriteBytesToHexLower(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => TryWriteBytesToHex(bytes, _lookup32LowerPtr, destination, out charsWritten);
    public static bool TryWriteBytesToHexUpper(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => TryWriteBytesToHex(bytes, _lookup32UpperPtr, destination, out charsWritten);
#endif

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
