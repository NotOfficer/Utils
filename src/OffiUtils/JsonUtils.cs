using System.Buffers;
using System.Diagnostics;
using System.Reflection;
using System.Text;

namespace OffiUtils;

public static class JsonUtils
{
	public const int StackallocByteThreshold = 256;
	public static readonly UTF8Encoding Encoding = new(encoderShouldEmitUTF8Identifier: false, throwOnInvalidBytes: true);

	private static readonly TryUnescapeDelegate TryUnescapeDelegate;

	static JsonUtils()
	{
		var helperType = Type.GetType("System.Text.Json.JsonReaderHelper, System.Text.Json")!;
		var tryUnescapeTypes = new[] { typeof(ReadOnlySpan<byte>), typeof(Span<byte>), typeof(int).MakeByRefType() };
		var tryUnescapeMethodInfo = helperType.GetMethod("TryUnescape", BindingFlags.Static | BindingFlags.NonPublic, tryUnescapeTypes)!;
		TryUnescapeDelegate = tryUnescapeMethodInfo.CreateDelegate<TryUnescapeDelegate>();
	}

	public static bool TryUnescape(ReadOnlySpan<byte> source, Span<byte> destination, out int written) => TryUnescapeDelegate(source, destination, out written);

	public static string GetUnescapedString(ReadOnlySpan<byte> utf8Source)
	{
		// The escaped name is always >= than the unescaped, so it is safe to use escaped name for the buffer length.
		var length = utf8Source.Length;
		byte[]? pooledName = null;
 
		var utf8Unescaped = length <= StackallocByteThreshold ?
			stackalloc byte[StackallocByteThreshold] :
			pooledName = ArrayPool<byte>.Shared.Rent(length);
 
		TryUnescapeDelegate(utf8Source, utf8Unescaped, out var written);
		Debug.Assert(written > 0);
 
		utf8Unescaped = utf8Unescaped[..written];
		Debug.Assert(!utf8Unescaped.IsEmpty);
 
		var utf8String = Encoding.GetString(utf8Unescaped);
 
		if (pooledName is not null)
		{
			utf8Unescaped.Clear();
			ArrayPool<byte>.Shared.Return(pooledName);
		}
 
		return utf8String;
	}
}