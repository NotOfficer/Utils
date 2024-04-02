using System.Diagnostics;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security.Cryptography;

using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils;

public static class StringUtils
{
	public const string RandomPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz";

	private static readonly nint Lookup32LowerPtr;
	private static readonly nint Lookup32UpperPtr;

	private static readonly FastAllocateStringDelegate FastAllocateStringDelegate;
	private static readonly GetRawStringDataDelegate GetRawStringDataDelegate;

	static StringUtils()
	{
		Lookup32LowerPtr = CreateLookup32Unsafe(true);
		Lookup32UpperPtr = CreateLookup32Unsafe(false);

		var stringType = typeof(string);
		var fastAllocateStringMethodInfo = stringType.GetMethod("FastAllocateString", BindingFlags.Static | BindingFlags.NonPublic)!;
		FastAllocateStringDelegate = fastAllocateStringMethodInfo.CreateDelegate<FastAllocateStringDelegate>();
		var getRawStringDataMethodInfo = stringType.GetMethod("GetRawStringData", BindingFlags.Instance | BindingFlags.NonPublic)!;
		GetRawStringDataDelegate = getRawStringDataMethodInfo.CreateDelegate<GetRawStringDataDelegate>();
	}

	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	private static char ConvertNibble(int nibble, int adjust) =>
		(char)(55 + adjust + nibble + (((nibble - 10) >> 31) & (-7 - adjust)));

	private static nint CreateLookup32Unsafe(bool lowerCase)
	{
		var adjust = lowerCase ? 32 : 0;
		var span = MemoryUtils.AllocNative<uint>(256, out var ptr);

		for (var i = 0; i < 256; i++)
		{
			var a = ConvertNibble(i >> 4, adjust);
			var b = ConvertNibble(i & 0xF, adjust);
			span[i] = ((uint)a) + ((uint)b << 16);
		}

		return ptr;
	}

	public static Span<char> GetSpan(string value) => MemoryMarshal.CreateSpan(ref GetRawStringDataDelegate(value), value.Length);

	public static string FastAllocate(int length) => FastAllocateStringDelegate(length);

	private static string BytesToHex(ReadOnlySpan<byte> bytes, nint lookupPtr)
	{
		var result = FastAllocateStringDelegate(bytes.Length * sizeof(char));
		var resultSpan32 = MemoryMarshal.CreateSpan(ref Unsafe.As<char, uint>(ref GetRawStringDataDelegate(result)), bytes.Length);
		var lookupSpan = MemoryUtils.GetSpan<uint>(lookupPtr, 256);

		for (var i = 0; i < resultSpan32.Length; i++)
		{
			resultSpan32[i] = lookupSpan[bytes[i]];
		}

		return result;
	}

	public static string BytesToHexLower(ReadOnlySpan<byte> bytes) => BytesToHex(bytes, Lookup32LowerPtr);
	public static string BytesToHexUpper(ReadOnlySpan<byte> bytes) => BytesToHex(bytes, Lookup32UpperPtr);

	public static ref char GetRawData(string instance) => ref GetRawStringDataDelegate(instance);

	public static string Random(int length, ReadOnlySpan<char> pool)
	{
		if (pool.IsEmpty) return string.Empty;
		var result = FastAllocateStringDelegate(length);
		var span = GetSpan(result);

		for (var i = 0; i < length; i++)
		{
			span[i] = pool[RandomNumberGenerator.GetInt32(pool.Length)];
		}

		return result;
	}
	public static string Random(int length) => Random(length, RandomPool);

	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	public static char ToLowerAsciiInvariant(char c)
	{
		if (char.IsAsciiLetterUpper(c))
		{
			// on x86, extending BYTE -> DWORD is more efficient than WORD -> DWORD
			c = (char)(byte)(c | 0x20);
		}
		return c;
	}

	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	internal static char ToUpperAsciiInvariant(char c)
	{
		if (char.IsAsciiLetterLower(c))
		{
			c = (char)(c & 0x5F); // = low 7 bits of ~0x20
		}
		return c;
	}
		
	public static void ToLowerAsciiInvariant(ReadOnlySpan<char> source, Span<char> destination)
	{
		Debug.Assert(destination.Length >= source.Length);

		for (var i = 0; i < source.Length; i++)
		{
			destination[i] = ToLowerAsciiInvariant(source[i]);
		}
	}

	public static void ToUpperAsciiInvariant(ReadOnlySpan<char> source, Span<char> destination)
	{
		Debug.Assert(destination.Length >= source.Length);

		for (var i = 0; i < source.Length; i++)
		{
			destination[i] = ToUpperAsciiInvariant(source[i]);
		}
	}

	public static void ToLowerAsciiInvariant(string value)
	{
		if (string.IsNullOrEmpty(value)) return;
		var span = GetSpan(value);
		ToLowerAsciiInvariant(span, span);
	}

	public static void ToUpperAsciiInvariant(string value)
	{
		if (string.IsNullOrEmpty(value)) return;
		var span = GetSpan(value);
		ToUpperAsciiInvariant(span, span);
	}

	public static string RealClone(string value)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var clone = FastAllocateStringDelegate(value.Length);
		value.AsSpan().CopyTo(GetSpan(clone));
		return clone;
	}

	public static string? CutAfter(string value, char needle, StringPool? pool = null)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var span = value.AsSpan();
		var index = span.IndexOf(needle);
		if (index == -1) return null;
		var result = span[(index + 1)..];
		return pool is null ? result.ToString() : pool.GetOrAdd(result);
	}

	public static string? CutAfterLast(string value, char needle, StringPool? pool = null)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var span = value.AsSpan();
		var index = span.LastIndexOf(needle);
		if (index == -1) return null;
		var result = span[(index + 1)..];
		return pool is null ? result.ToString() : pool.GetOrAdd(result);
	}

	public static string? CutAfter(string value, string needle, StringComparison comparisonType = StringComparison.Ordinal, StringPool? pool = null)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var span = value.AsSpan();
		var index = span.IndexOf(needle, comparisonType);
		if (index == -1) return null;
		var result = span[(index + 1)..];
		return pool is null ? result.ToString() : pool.GetOrAdd(result);
	}

	public static string? CutAfterLast(string value, string needle, StringComparison comparisonType = StringComparison.Ordinal, StringPool? pool = null)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var span = value.AsSpan();
		var index = span.LastIndexOf(needle, comparisonType);
		if (index == -1) return null;
		var result = span[(index + 1)..];
		return pool is null ? result.ToString() : pool.GetOrAdd(result);
	}

	public static string? CutBefore(string value, char needle, StringPool? pool = null)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var span = value.AsSpan();
		var index = span.IndexOf(needle);
		if (index == -1) return null;
		var result = span[..index];
		return pool is null ? result.ToString() : pool.GetOrAdd(result);
	}

	public static string? CutBeforeLast(string value, char needle, StringPool? pool = null)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var span = value.AsSpan();
		var index = span.LastIndexOf(needle);
		if (index == -1) return null;
		var result = span[..index];
		return pool is null ? result.ToString() : pool.GetOrAdd(result);
	}

	public static string? CutBefore(string value, string needle, StringComparison comparisonType = StringComparison.Ordinal, StringPool? pool = null)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var span = value.AsSpan();
		var index = span.IndexOf(needle, comparisonType);
		if (index == -1) return null;
		var result = span[..index];
		return pool is null ? result.ToString() : pool.GetOrAdd(result);
	}

	public static string? CutBeforeLast(string value, string needle, StringComparison comparisonType = StringComparison.Ordinal, StringPool? pool = null)
	{
		if (string.IsNullOrEmpty(value)) return string.Empty;
		var span = value.AsSpan();
		var index = span.LastIndexOf(needle, comparisonType);
		if (index == -1) return null;
		var result = span[..index];
		return pool is null ? result.ToString() : pool.GetOrAdd(result);
	}
}
