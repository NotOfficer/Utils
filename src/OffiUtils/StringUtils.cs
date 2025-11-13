using System.Runtime.CompilerServices;
using System.Security.Cryptography;

namespace OffiUtils;

public static partial class StringUtils
{
	public const string RandomPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890123456789012345678901234567890123456789";

	static StringUtils()
	{
#if !MS_IS_STUPID
		Lookup32LowerPtr = CreateLookup32Unsafe(true);
		Lookup32UpperPtr = CreateLookup32Unsafe(false);
#endif
	}

	private static void FastAllocateCreateAction(Span<char> str, int arg) { }

	public static string FastAllocate(int length) => string.Create(length, length, FastAllocateCreateAction);

#if MS_IS_STUPID // https://github.com/dotnet/runtime/issues/109807
	public static string BytesToHexLower(ReadOnlySpan<byte> bytes) => Convert.ToHexStringLower(bytes);
	public static string BytesToHexUpper(ReadOnlySpan<byte> bytes) => Convert.ToHexString(bytes);

	public static bool TryWriteBytesToHexLower(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => Convert.TryToHexStringLower(bytes, destination, out charsWritten);
	public static bool TryWriteBytesToHexUpper(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => Convert.TryToHexString(bytes, destination, out charsWritten);
#else
	private static readonly nint Lookup32LowerPtr;
	private static readonly nint Lookup32UpperPtr;

	private static char ConvertNibble(int nibble, int adjust) =>
		(char)(55 + adjust + nibble + (((nibble - 10) >> 31) & (-7 - adjust)));

	private static nint CreateLookup32Unsafe(bool lowerCase)
	{
		var adjust = lowerCase ? 32 : 0;
		var span = MemoryUtils.NativeAlloc<uint>(256, out var ptr);

		for (var i = 0; i < 256; i++)
		{
			var a = ConvertNibble(i >> 4, adjust);
			var b = ConvertNibble(i & 0xF, adjust);
			span[i] = ((uint)a) + ((uint)b << 16);
		}

		return ptr;
	}

	private static string BytesToHex(ReadOnlySpan<byte> bytes, nint lookupPtr)
	{
		var result = FastAllocate(bytes.Length * sizeof(char));
		var resultSpan32 = System.Runtime.InteropServices.MemoryMarshal.CreateSpan(ref Unsafe.As<char, uint>(ref result.GetRawData()), bytes.Length);
		var lookupSpan = MemoryUtils.GetSpan<uint>(lookupPtr, 256);

		for (var i = 0; i < resultSpan32.Length; i++)
		{
			resultSpan32[i] = lookupSpan[bytes[i]];
		}

		return result;
	}

	private static bool TryWriteBytesToHex(ReadOnlySpan<byte> bytes, nint lookupPtr, Span<char> destination, out int charsWritten)
	{
		var charsToWrite = bytes.Length * 2;
		if (destination.Length < charsToWrite)
		{
			charsWritten = 0;
			return false;
		}

		var resultSpan32 = System.Runtime.InteropServices.MemoryMarshal.CreateSpan(ref Unsafe.As<char, uint>(ref destination.GetPinnableReference()), bytes.Length);
		var lookupSpan = MemoryUtils.GetSpan<uint>(lookupPtr, 256);

		for (var i = 0; i < resultSpan32.Length; i++)
		{
			resultSpan32[i] = lookupSpan[bytes[i]];
		}

		charsWritten = charsToWrite;
		return true;
	}

	public static string BytesToHexLower(ReadOnlySpan<byte> bytes) => BytesToHex(bytes, Lookup32LowerPtr);
	public static string BytesToHexUpper(ReadOnlySpan<byte> bytes) => BytesToHex(bytes, Lookup32UpperPtr);

	public static bool TryWriteBytesToHexLower(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => TryWriteBytesToHex(bytes, Lookup32LowerPtr, destination, out charsWritten);
	public static bool TryWriteBytesToHexUpper(ReadOnlySpan<byte> bytes, Span<char> destination, out int charsWritten) => TryWriteBytesToHex(bytes, Lookup32UpperPtr, destination, out charsWritten);
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
		for (var i = 0; i < value.Length; i++)
		{
			destination[i] = ToLowerAsciiInvariant(value[i]);
		}
	}

	public static void ToUpperAsciiInvariant(ReadOnlySpan<char> value, Span<char> destination)
	{
		for (var i = 0; i < value.Length; i++)
		{
			destination[i] = ToUpperAsciiInvariant(value[i]);
		}
	}

	public static void ToLowerAsciiInvariant(string value)
	{
		if (value.Length == 0) return;
		var span = value.GetSpan();
		ToLowerAsciiInvariant(span, span);
	}

	public static void ToUpperAsciiInvariant(string value)
	{
		if (value.Length == 0) return;
		var span = value.GetSpan();
		ToUpperAsciiInvariant(span, span);
	}

	public static string RealClone(ReadOnlySpan<char> value)
		=> value.ToString();
}
