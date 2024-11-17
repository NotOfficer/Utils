using System.Buffers;
using System.Runtime.InteropServices;

namespace OffiUtils;

public abstract class RandomAccessStream : Stream
{
	public abstract int ReadAt(long position, byte[] buffer, int offset, int count);

	public virtual int ReadAt(long position, Span<byte> buffer)
	{
		var sharedBuffer = ArrayPool<byte>.Shared.Rent(buffer.Length);
		try
		{
			var numRead = ReadAt(position, sharedBuffer, 0, buffer.Length);
			new ReadOnlySpan<byte>(sharedBuffer, 0, numRead).CopyTo(buffer);
			return numRead;
		}
		finally
		{
			ArrayPool<byte>.Shared.Return(sharedBuffer);
		}
	}

	public abstract Task<int> ReadAtAsync(long position, byte[] buffer, int offset, int count, CancellationToken cancellationToken = default);

	public virtual ValueTask<int> ReadAtAsync(long position, Memory<byte> buffer, CancellationToken cancellationToken = default)
	{
		if (MemoryMarshal.TryGetArray(buffer, out ArraySegment<byte> array))
		{
			return new ValueTask<int>(ReadAtAsync(position, array.Array!, array.Offset, array.Count, cancellationToken));
		}

		var sharedBuffer = ArrayPool<byte>.Shared.Rent(buffer.Length);
		return FinishReadAtAsync(ReadAtAsync(position, sharedBuffer, 0, buffer.Length, cancellationToken), sharedBuffer, buffer);

		static async ValueTask<int> FinishReadAtAsync(Task<int> readAtTask, byte[] localBuffer, Memory<byte> localDestination)
		{
			try
			{
				var result = await readAtTask.ConfigureAwait(false);
				new ReadOnlySpan<byte>(localBuffer, 0, result).CopyTo(localDestination.Span);
				return result;
			}
			finally
			{
				ArrayPool<byte>.Shared.Return(localBuffer);
			}
		}
	}
}
