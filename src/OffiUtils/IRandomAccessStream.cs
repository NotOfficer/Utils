namespace OffiUtils;

public interface IRandomAccessStream
{
	public bool CanRead { get; }
	public bool CanWrite { get; }
	public bool CanSeek { get; }

	public long Length { get; }
	public long Position { get; set; }

	public long Seek(long offset, SeekOrigin origin);
	public int Read(byte[] buffer, int offset, int count);

	public int ReadAt(long position, byte[] buffer, int offset, int count);
	public Task<int> ReadAtAsync(long position, byte[] buffer, int offset, int count, CancellationToken cancellationToken);
	public Task<int> ReadAtAsync(long position, Memory<byte> memory, CancellationToken cancellationToken);
}
