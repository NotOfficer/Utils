using System.Buffers;
using System.Text;
using System.Text.Json;

namespace OffiUtils;

public static unsafe class JsonUtils
{
	public const int StackallocByteThreshold = 256;
	public static readonly UTF8Encoding Encoding = new(encoderShouldEmitUTF8Identifier: false, throwOnInvalidBytes: true);

	public static bool TryUnescape(ReadOnlySpan<byte> utf8Source, Span<byte> destination, out int written)
	{
		byte[]? pooledName = null;

		utf8Source = utf8Source.Trim((byte)'"');

		var length = utf8Source.Length + 2 /* [" */ + 2 /* "] */;

		var arraySource = length <= StackallocByteThreshold ?
			stackalloc byte[length] :
			(pooledName = ArrayPool<byte>.Shared.Rent(length))
				.AsSpan()[..length];

		try
		{
			arraySource[0] = (byte)'[';
			arraySource[1] = (byte)'"';

			utf8Source.CopyTo(arraySource[2..^2]);

			arraySource[^2] = (byte)'"';
			arraySource[^1] = (byte)']';

			return TryUnescapeFromJsonArray(arraySource, destination, out written);
		}
		finally
		{
			if (pooledName is not null)
			{
				arraySource.Clear();
				ArrayPool<byte>.Shared.Return(pooledName);
			}
		}
	}

	public static bool TryUnescape(ReadOnlySpan<byte> utf8Source, Span<char> destination, out int written)
	{
		byte[]? pooledName = null;

		utf8Source = utf8Source.Trim((byte)'"');

		var length = utf8Source.Length + 2 /* [" */ + 2 /* "] */;

		var arraySource = length <= StackallocByteThreshold ?
			stackalloc byte[length] :
			(pooledName = ArrayPool<byte>.Shared.Rent(length))
			.AsSpan()[..length];

		try
		{
			arraySource[0] = (byte)'[';
			arraySource[1] = (byte)'"';

			utf8Source.CopyTo(arraySource[2..^2]);

			arraySource[^2] = (byte)'"';
			arraySource[^1] = (byte)']';

			return TryUnescapeFromJsonArray(arraySource, destination, out written);
		}
		finally
		{
			if (pooledName is not null)
			{
				arraySource.Clear();
				ArrayPool<byte>.Shared.Return(pooledName);
			}
		}
	}

	private static bool TryUnescapeFromJsonArray(ReadOnlySpan<byte> arraySource, Span<byte> destination, out int written)
	{
		written = 0;

		try
		{
			var reader = new Utf8JsonReader(arraySource);

			var arrayStartResult = reader.Read();
			if (!arrayStartResult || reader.TokenType != JsonTokenType.StartArray)
			{
				return false;
			}

			var stringEntryResult = reader.Read();
			if (!stringEntryResult || reader.TokenType != JsonTokenType.String)
			{
				return false;
			}

			written = reader.CopyString(destination);
			return written != 0;
		}
		catch
		{
			return false;
		}
	}

	private static bool TryUnescapeFromJsonArray(ReadOnlySpan<byte> arraySource, Span<char> destination, out int written)
	{
		written = 0;

		try
		{
			var reader = new Utf8JsonReader(arraySource);

			var arrayStartResult = reader.Read();
			if (!arrayStartResult || reader.TokenType != JsonTokenType.StartArray)
			{
				return false;
			}

			var stringEntryResult = reader.Read();
			if (!stringEntryResult || reader.TokenType != JsonTokenType.String)
			{
				return false;
			}

			written = reader.CopyString(destination);
			return written != 0;
		}
		catch
		{
			return false;
		}
	}

	private static string UnescapeFromJsonArray(ReadOnlySpan<byte> arraySource)
	{
		try
		{
			var reader = new Utf8JsonReader(arraySource);

			var arrayStartResult = reader.Read();
			if (!arrayStartResult || reader.TokenType != JsonTokenType.StartArray)
			{
				return "";
			}

			var stringEntryResult = reader.Read();
			if (!stringEntryResult || reader.TokenType != JsonTokenType.String)
			{
				return "";
			}

			return reader.GetString()!; // only null when TokenType == JsonTokenType.Null. JsonTokenType.String is ensured
		}
		catch
		{
			return "";
		}
	}

	public static string GetUnescapedString(ReadOnlySpan<byte> utf8Source)
	{
		byte[]? pooledName = null;

		utf8Source = utf8Source.Trim((byte)'"');

		var length = utf8Source.Length + 2 /* [" */ + 2 /* "] */;

		var arraySource = length <= StackallocByteThreshold ?
			stackalloc byte[length] :
			(pooledName = ArrayPool<byte>.Shared.Rent(length))
			.AsSpan()[..length];

		try
		{
			arraySource[0] = (byte)'[';
			arraySource[1] = (byte)'"';

			utf8Source.CopyTo(arraySource[2..^2]);

			arraySource[^2] = (byte)'"';
			arraySource[^1] = (byte)']';

			return UnescapeFromJsonArray(arraySource);
		}
		finally
		{
			if (pooledName is not null)
			{
				arraySource.Clear();
				ArrayPool<byte>.Shared.Return(pooledName);
			}
		}
	}
}
